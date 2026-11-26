(ns hive-tmux.claude.goal-pane
  "A fake host port: one Claude Code pane (%9) whose /goal keeps ONE turn
   alive forever, the way a coordinator looping on 100 s `ss wait` calls does
   (friction 20261121180326-2ea1d54b). It never shows a turn boundary on its
   own. It reacts to keys the way Claude Code 2.1.330 was measured to:

     /compact + Enter while busy  -> queued (`Queued · ctrl+x ctrl+s to send now`)
     C-x C-s with a queued line   -> the running tool moves to the background,
                                     the queued line runs now
     Escape while busy            -> the turn stops (in INSERT mode the first
                                     Esc only leaves insert mode)
     /compact at rest             -> `Compacting conversation…` for N reads,
                                     then the context drops and the goal clears

   :tool is the running tool call the screen draws (`ss-wait`, `ship-land`
   or nil for thinking). :write-cut records a write tool that a key killed.
   No tmux, no clock: :sleep! advances a fake clock. Used by the tests and
   by the hive-assay arm runner (dev/assay_compact_interrupt.clj)."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def goal-text "Run the support loop: wait on ss, answer asks, recycle panes.")

(defn goal-line [args]
  (json/write-str {:type "user"
                   :message {:role "user"
                             :content (str "<command-name>/goal</command-name>\n"
                                           "<command-message>goal</command-message>\n"
                                           "<command-args>" args "</command-args>")}}))

(defn goal-status-line
  "The `goal_status` attachment Claude Code 2.1.330 writes when it checks a
   goal (copied from session 15c7760e, whose goal left no /goal entry)."
  ([condition] (goal-status-line condition false))
  ([condition met?]
   (json/write-str {:type "attachment"
                    :attachment {:type "goal_status" :met met? :sentinel true :condition condition}})))

(def tools
  {:ss-wait ["● hive - swarm (MCP)(command: \"ss wait\", parent: \"coordinator:ce1435b2\", timeout_ms: 100000)"
             "  ⎿  Running…"]
   :ship-land ["● hive - git (MCP)(command: \"ship land\", directory: \"/srv/peer\", base: \"staging\","
               "  merge: \"true\", wait_s: \"1500\")"
               "  ⎿  Running…"]
   :sleep ["● Bash(timeout 110 sleep 100; echo done)"
           "  ⎿  Running… (12s · timeout 2m)"]})

(defn- write? [tool] (= :ship-land tool))

(defn screen [{:keys [tokens busy? goal? tool queued compacting typed vim human] :as s}]
  (vec (concat
        ["● Support loop: still waiting on the lanes."]
        (when (and busy? tool) (get tools tool))
        (when (seq queued) (conj (mapv #(str "\u001b[38;5;246m\u001b[48;5;237m❯ " % "\u001b[39m") queued)
                                 "\u001b[49m  ⎿  Queued · ctrl+x ctrl+s to send now"))
        [(cond (pos? (or compacting 0)) "✢ Compacting conversation… (3s · ↓ 211 tokens)"
               busy? "✻ Calculating… (1h 49m 53s · ↓ 118.9k tokens)"
               :else "✻ Worked for 2s")
         "  ⎿  Tip: Use /btw to ask a side question without interrupting Claude"
         (str "   " tokens " tokens" (when goal? " · ◎ /goal active (2h)"))
         "──────────── peer one ─"
         (str "❯\u00a0" (or human
                            ;; Pane %8 (job cc-a49d0119): with a human message
                            ;; queued, the composer read back that message,
                            ;; not the /compact just typed.
                            (when (and typed (seq queued) (:recall-queued? s)) (first queued))
                            typed
                            (when (seq queued) "\u001b[2mPress up to edit queued messages\u001b[0m")))
         "────────────"
         (str "  " (if (= :normal vim) "-- NORMAL --" "-- INSERT --") " ⏵⏵ auto mode on")])))

(defn- compact-now [s]
  (-> s (assoc :busy? false :tool nil :queued nil :compacting (:compact-reads s 3))
      (update :sent conj "/compact")))

(defn- on-read
  "One screen read: a running compaction advances; a turn restarted by the
   Stop hook keeps the pane busy."
  [s]
  (let [c (or (:compacting s) 0)]
    (cond (> c 1) (update s :compacting dec)
          (= c 1) (-> s (assoc :compacting 0 :tokens (:tokens-after s 30000) :goal? false)
                      (update :transcript conj
                              (json/write-str {:type "system" :subtype "compact_boundary"
                                               :compactMetadata {:preTokens (:tokens s) :postTokens 30000}})))
          (and (:busy-reads s) (pos? (:busy-reads s)))
          (let [n (dec (:busy-reads s))] (cond-> (assoc s :busy-reads n) (zero? n) (assoc :busy? false :tool nil)))
          :else s)))

(defn- submit [s]
  (let [line (str/trim (str (:typed s)))
        s (assoc s :typed nil)]
    (cond
      (str/blank? line) s
      (or (:busy? s) (pos? (or (:compacting s) 0))) (update s :queued (fnil conj []) line)
      (str/starts-with? line "/compact") (compact-now (assoc s :typed nil))
      :else (-> s (update :sent conj line)
                (assoc :busy? true :tool nil :busy-reads 2)
                (cond-> (str/starts-with? line "/goal ")
                  ;; Claude Code writes the /goal entry to the transcript.
                  (-> (assoc :goal? true) (update :transcript (fnil conj []) (goal-line (subs line 6)))))))))

(defn- key! [s k]
  (case k
    "Enter" (submit s)
    "C-End" s
    "i" (if (= :normal (:vim s)) (assoc s :vim :insert) (update s :typed str "i"))
    "Escape" (cond (= :insert (:vim s)) (assoc s :vim :normal)
                   (:busy? s) (-> s (assoc :busy? false :escaped true)
                                  (cond-> (write? (:tool s)) (update :write-cut (fnil inc 0)))
                                  (assoc :tool nil))
                   :else s)
    s))

(defn- chord! [s ks]
  (if (and (= ["C-x" "C-s"] ks) (seq (:queued s)))
    ;; Every queued line goes out now, in order; the human's land in :sent
    ;; and in :human-delivered, and the turn goes on with them.
    (let [lines (:queued s)
          s (-> s (assoc :queued nil :backgrounded (:tool s)))
          compact? (some #(str/starts-with? % "/compact") lines)
          s (reduce (fn [s l] (if (str/starts-with? l "/compact")
                                s
                                (-> s (update :sent conj l) (update :human-delivered (fnil conj []) l))))
                    s lines)]
      (if compact? (compact-now s) s))
    (reduce key! s ks)))

(defn goal-pane
  "{:state atom :host port}. OPTS: :tokens (default 540000), :tool (the
   running call, default :ss-wait), :vim (:insert), :busy? (true), :goal?
   (true), :compact-reads (3), :human (a draft at the prompt), :queued (a
   vector of messages someone SUBMITTED while the turn ran, waiting in the
   queue), :recall-queued? (the composer reads back the first queued
   message while one waits, as pane %8 did), :transcript, :tick (a fn
   of the pane state run before every screen read: a scripted timeline that
   changes the running tool or ends the turn, keyed on :clock)."
  ([] (goal-pane {}))
  ([opts]
   (let [st (atom (merge {:tokens 540000 :tool :ss-wait :vim :insert :busy? true :goal? true
                          :clock 0 :sent [] :keys [] :typed nil
                          :transcript ["{\"type\":\"x\"}" (goal-line goal-text)]}
                         opts))]
     {:state st
      :host
      {:tmux! (fn tmux!
                ([argv] (tmux! argv nil))
                ([argv _stdin]
                 (case (first argv)
                   "list-panes" {:exit 0 :out "%9\t1\t3\t1\t100\tclaude\t/srv/peer\t✳ peer one\t/dev/pts/9\n" :err ""}
                   "capture-pane" (let [s (swap! st (comp on-read (:tick opts identity)))] {:exit 0 :out (str/join "\n" (screen s)) :err ""})
                   "send-keys" (let [[_ _ _ & ks] argv]
                                 (swap! st update :keys conj (vec ks))
                                 (if (= "-l" (first ks))
                                   (swap! st update :typed str (second ks))
                                   (swap! st chord! (vec ks)))
                                 {:exit 0 :out "" :err ""})
                   {:exit 0 :out "" :err ""})))
       :sessions (fn [] [{:pid 200 :sessionId "sid-1" :name "peer one" :cwd "/srv/peer"
                          :status (if (:busy? @st) "busy" "idle") :updatedAt 1}])
       :transcript (fn [cwd sid] (when (= ["/srv/peer" "sid-1"] [cwd sid]) (:transcript @st)))
       :sleep! (fn [ms] (swap! st update :clock + ms))
       :now-ms (fn [] (:clock @st))}})))

(ns hive-tmux.claude.stop-hook-pane
  "A fake host port: one Claude Code pane (%8) whose /goal Stop hook
   re-prompts at every turn end, drawn and keyed the way Claude Code 2.1.330
   was measured to on a scratch pane (2026-11-22, card 20261122170735-7c6f5ad3):

     every read          the spinner never goes away; a turn runs, then the
                         spinner says `(running Stop hooks… 1/2 · …)` for one
                         read, then `◯ Goal not yet met… continuing` and the
                         next turn. No read is ever idle.
     /compact + Enter    Queued (`⎿ Queued · ctrl+x ctrl+s to send now`); it
                         stays queued across every re-prompt.
     /goal clear + Enter taken in at once, mid-turn: the footer drops the goal;
                         the running turn then ends, the pane rests, and a
                         queued /compact runs.
     /compact at rest    `Compacting conversation…` for :compact-reads reads,
                         then the context drops and a compact_boundary lands.
     /goal <text>        the goal is set (footer, transcript entry) and the
                         loop starts again.
     any other prompt    a turn of :turn-reads reads.

   No tmux, no clock: :sleep! advances a fake clock. :sent records every
   submitted line, :typed-while-busy every non-/goal text typed while busy."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def goal-text "Run the support loop until every lane is merged: wait on ss, answer asks, recycle panes.")

(defn goal-line [args]
  (json/write-str {:type "user"
                   :message {:role "user"
                             :content (str "<command-name>/goal</command-name>\n"
                                           "<command-message>goal</command-message>\n"
                                           "<command-args>" args "</command-args>")}}))

(defn screen
  "The pane's lines for state S."
  [{:keys [tokens goal? phase queued typed human compacting]}]
  (vec (concat
        ["● tick"]
        (when (= :continuing phase)
          ["◯ Goal not yet met… continuing"
           (str "  Goal: " goal-text)
           "  Reason: The lanes are not merged yet."])
        (when (seq queued)
          (conj (mapv #(str "❯ " %) queued) "  ⎿  Queued · ctrl+x ctrl+s to send now"))
        [(cond (pos? (or compacting 0)) "✢ Compacting conversation… (3s · ↓ 211 tokens)"
               (= :hook phase) "* Pouncing… (running Stop hooks… 1/2 · 2m 21s · ↓ 2.0k tokens)"
               (= :rest phase) "✻ Crunched for 4m 29s"
               :else "✻ Pouncing… (2m 24s · ↓ 2.0k tokens)")
         (str "   " tokens " tokens" (when goal? " · ◎ /goal active (2h)"))
         "──────────── coordinator one ─"
         (str "❯\u00a0" (or human typed
                            (when (seq queued) "\u001b[2mPress up to edit queued messages\u001b[0m")))
         "────────────"
         "  -- INSERT -- ⏵⏵ auto mode on"])))

(defn- boundary [s]
  (json/write-str {:type "system" :subtype "compact_boundary"
                   :compactMetadata {:preTokens (:tokens s) :postTokens 30000}}))

(defn- start-queued
  "At rest with something queued: Claude Code runs it now."
  [s]
  (let [[q & more] (:queued s)]
    (cond (nil? q) s
          (str/starts-with? q "/compact")
          (-> s (assoc :queued (vec more) :compacting (:compact-reads s 3) :phase :compacting)
              (update :sent conj q))
          :else (-> s (assoc :queued (vec more) :phase :turn :left (:turn-reads s 2))
                    (update :sent conj q)))))

(defn on-read
  "One screen read advances the pane one step."
  [s]
  (let [s (update s :reads (fnil inc 0))]
    (cond
      (pos? (or (:compacting s) 0))
      (if (> (:compacting s) 1)
        (update s :compacting dec)
        (-> s (assoc :compacting 0 :phase :rest :tokens 30000)
            (update :transcript conj (boundary s))))

      (= :rest (:phase s)) (start-queued s)

      ;; A turn runs :left more reads, then ends: under the goal the Stop hook
      ;; runs and re-prompts; without it the pane rests.
      (= :turn (:phase s))
      (if (pos? (dec (or (:left s) 1)))
        (update s :left dec)
        (assoc s :phase (if (:goal? s) :hook :rest)))

      (= :hook (:phase s)) (if (:goal? s) (assoc s :phase :continuing) (assoc s :phase :rest))
      (= :continuing (:phase s)) (assoc s :phase :turn :left (:turn-reads s 2))
      :else s)))

(defn- busy? [s] (not= :rest (:phase s)))

(defn- submit [s]
  (let [line (str/trim (str (:typed s)))
        s (assoc s :typed nil)]
    (cond
      (str/blank? line) s
      ;; /goal is taken in at once, even mid-turn.
      (#{"/goal clear" "/goal off" "/goal stop"} line)
      (-> s (assoc :goal? false) (update :sent conj line)
          (update :transcript conj (goal-line "clear")))
      (str/starts-with? line "/goal ")
      (-> s (assoc :goal? true) (update :sent conj line)
          (update :transcript conj (goal-line (subs line 6)))
          (cond-> (not (busy? s)) (assoc :phase :turn :left (:turn-reads s 2))))
      (busy? s) (update s :queued (fnil conj []) line)
      :else (start-queued (update s :queued (fnil conj []) line)))))

(defn stop-hook-pane
  "{:state atom :host port}. OPTS: :tokens (default 470000), :goal? (true),
   :phase (:hook), :turn-reads (2), :compact-reads (3), :human (a draft at
   the prompt), :transcript."
  ([] (stop-hook-pane {}))
  ([opts]
   (let [st (atom (merge {:tokens 470000 :goal? true :phase :hook :turn-reads 2 :compact-reads 3
                          :clock 0 :sent [] :typed nil :queued [] :typed-while-busy 0
                          :transcript ["{\"type\":\"x\"}" (goal-line goal-text)]}
                         opts))]
     {:state st
      :host
      {:tmux! (fn tmux!
                ([argv] (tmux! argv nil))
                ([argv _stdin]
                 (case (first argv)
                   "list-panes" {:exit 0 :err ""
                                 :out "%8\t1\t3\t1\t100\tclaude\t/srv/coord\t✳ coordinator one\t/dev/pts/8\n"}
                   "capture-pane" (let [s (swap! st on-read)] {:exit 0 :err "" :out (str/join "\n" (screen s))})
                   "send-keys" (let [[_ _ _ & ks] argv]
                                 (cond
                                   (= "-l" (first ks))
                                   (swap! st (fn [s] (cond-> (update s :typed str (second ks))
                                                       (and (busy? s) (not (str/starts-with? (str (:typed s) (second ks)) "/goal")))
                                                       (update :typed-while-busy inc))))
                                   (= ["Enter"] ks) (swap! st submit))
                                 {:exit 0 :out "" :err ""})
                   {:exit 0 :out "" :err ""})))
       :sessions (fn [] [{:pid 100 :sessionId "6fe2d4c2-aaaa" :name "coordinator one" :cwd "/srv/coord"
                          :status (if (busy? @st) "busy" "idle") :updatedAt 1}])
       :transcript (fn [cwd sid] (when (= ["/srv/coord" "6fe2d4c2-aaaa"] [cwd sid]) (:transcript @st)))
       :sleep! (fn [ms] (swap! st update :clock + ms))
       :now-ms (fn [] (:clock @st))}})))

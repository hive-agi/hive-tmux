(ns hive-tmux.claude.compact-goal-hook-test
  "Compacting a pane whose /goal Stop hook keeps it working.

   Friction 20261116185504-0376516a: job cc-bed53604 on pane %5 waited 30 min
   at :wait-idle for a settled idle, failed :timeout, and the pane grew from
   337k to 469k tokens. A /goal Stop hook starts the next turn within a poll,
   so such a pane is at rest for ONE read between turns and never for two in a
   row: `poll/wait-settled-idle` can never confirm, and a job built on it can
   only time out.

   The host here is a stub built in this namespace: no tmux, no ~/.claude, no
   real clock, no network. Nothing is sent to a real pane."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.registry :as registry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def goal "Work in a loop on funeraria: take card 0812fc24, then 2a255f97.")

(def reseed "hive workflow catchup /caveman")

(defn- goal-line [args]
  (json/write-str {:type "user"
                   :message {:role "user"
                             :content (str "<command-name>/goal</command-name>\n"
                                           "<command-message>goal</command-message>\n"
                                           "<command-args>" args "</command-args>")}}))

(defn- pane-screen
  "The screen of the stub pane. REST? draws Claude Code's settled state (no
   spinner); otherwise the spinner line is there and the pane is working. The
   status line (context size, /goal) is drawn either way, as on a real pane."
  [{:keys [tokens goal? rest? typed]}]
  [(if rest?
     "\u273b Worked for 3m 12s"
     "\u2736 Percolating\u2026 (26m 48s \u00b7 \u2193 29.8k tokens)")
   (str "  " tokens " tokens" (when goal? " \u00b7 \u25ce /goal active (26m)"))
   "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500 peer one \u2500"
   (str "\u001b[39m\u276f " typed)
   "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500"
   "  -- INSERT -- \u23f5\u23f5 auto mode on"])

(defn- submit
  "The pane after LINE was submitted: recorded, and /compact | reseed | /goal
   applied to its context and goal the way Claude Code applies them."
  [s line]
  (let [s (-> s (update :sent conj line) (assoc :typed ""))]
    (cond-> s
      (str/starts-with? line "/compact") (assoc :tokens 21000 :goal? false)
      (str/starts-with? line "/goal ") (assoc :goal? true)
      (= line reseed) (update :tokens + 20000))))

(defn- stub
  "A host with one pane, %7 `peer one`, working continuously under an active
   /goal. REST-EVERY N means every Nth screen read finds it at rest and every
   other read finds it working -- ONE rest at a time, never two in a row, so a
   settled idle (idle twice, :settle-ms apart) never happens on it. REST-EVERY
   0 means it never rests at all.

   :sent records every submitted line in order, :reads counts screen reads,
   :clock is the ms the job slept, and :typed-while-busy counts the literal
   keys typed into the composer at a read that was NOT such a boundary -- that
   count is what \"never type into a working pane\" means here."
  [{:keys [tokens goal? transcript rest-every]
    :or {rest-every 3}}]
  (let [st (atom {:tokens tokens :goal? (boolean goal?) :typed "" :sent []
                  :reads 0 :clock 0 :transcript transcript :rest? false
                  :typed-while-busy 0})
        read-screen! (fn []
                       (swap! st (fn [s]
                                   (let [n (inc (:reads s))]
                                     (assoc s :reads n
                                            :rest? (and (pos? rest-every)
                                                        (zero? (mod n rest-every))))))))
        type! (fn [text]
                (swap! st (fn [s] (-> s
                                      (update :typed str text)
                                      ;; A /goal is accepted while a turn runs and
                                      ;; is typed without rest (card 70541f83).
                                      (cond-> (and (not (:rest? s)) (not (str/starts-with? text "/goal ")))
                                        (update :typed-while-busy inc))))))]
    {:state st
     :host
     {:tmux! (fn tmux!
               ([argv] (tmux! argv nil))
               ([argv stdin]
                (case (first argv)
                  "list-panes" {:exit 0 :err ""
                                :out "%7\t1\t1\t1\t4242\tclaude\t/srv/peer\t\u2733 peer one\n"}
                  "capture-pane" (let [s (read-screen!)]
                                   {:exit 0 :err "" :out (str/join "\n" (pane-screen s))})
                  "load-buffer" (do (swap! st assoc :buffer stdin) {:exit 0 :out "" :err ""})
                  "paste-buffer" (do (type! (:buffer @st)) {:exit 0 :out "" :err ""})
                  "send-keys" (let [[_ _ _ & ks] argv]
                                (cond (= ["Enter"] ks) (swap! st submit (:typed @st))
                                      (= "-l" (first ks)) (type! (second ks)))
                                {:exit 0 :out "" :err ""})
                  {:exit 0 :out "" :err ""})))
      ;; Claude Code's own session status: the screen spinner is the busy
      ;; signal this stub exercises, so the session file always reads idle.
      :sessions (fn [] [{:pid 4242 :sessionId "sid-1" :name "peer one" :cwd "/srv/peer"
                         :status "idle" :updatedAt 1}])
      :transcript (fn [cwd sid]
                    (when (= ["/srv/peer" "sid-1"] [cwd sid]) (:transcript @st)))
      :sleep! (fn [ms] (swap! st update :clock + ms))
      :now-ms (fn [] (:clock @st))}}))

(def ^:private fast {:poll-ms 1000 :settle-ms 2000 :sync? true})

(deftest a-goal-pane-is-compacted-between-turns-instead-of-timing-out
  ;; The falsifier for friction 20261116185504-0376516a. On the unfixed tree
  ;; this pane's job ends :failed (:timeout, :during :wait-idle) with not one
  ;; key sent; a compact path for busy goal panes ends it :done, with /compact
  ;; submitted between turns and the /goal restored after the re-seed.
  (let [{:keys [state host]} (stub {:tokens 469000 :goal? true
                                    :transcript [(goal-line goal)]})
        job (compact/start! "%7" (assoc fast :host host :idle-timeout-ms 60000))]
    (is (= :done (:status job))
        (str "a /goal Stop hook never settles, so the job must compact between turns; got "
             (pr-str (select-keys job [:status :reason :error]))))
    (is (= ["/compact" reseed (str "/goal " goal)] (:sent @state))
        "compacted, re-seeded, goal restored -- in that order")
    (is (true? (:goal-active-after? job)) "the pane is working again under its goal")
    (is (= goal (:goal job)) "the captured goal text is what came back")
    (is (< (:tokens-after job) 469000) "the context came down")
    (is (some #(= :compacted (:step %)) (:steps job))
        "the job recorded proof the compaction ran")
    (is (zero? (:typed-while-busy @state))
        "every key landed at a turn boundary, never on a running turn")
    (is (< (:clock @state) 60000) "and it did so inside the idle timeout")))

(deftest a-pane-that-never-settles-and-has-no-goal-still-times-out
  ;; The old path is left exactly as it was for a pane busy for any other
  ;; reason: settling is what keeps a send off a working turn.
  (let [{:keys [state host]} (stub {:tokens 469000 :goal? false})
        job (compact/start! "%7" (assoc fast :host host :idle-timeout-ms 20000))]
    (is (= [:failed :timeout] [(:status job) (:reason job)]))
    (is (= :wait-idle (get-in job [:error :during])))
    (is (= :settled-idle (get-in job [:error :waited-for])))
    (is (empty? (:sent @state)) "nothing is typed into a working pane")))

(deftest the-settled-idle-wait-can-still-be-demanded-explicitly
  ;; The escape hatch: between-turns? false keeps the old, stricter contract,
  ;; and its timeout still names what it waited for.
  (let [{:keys [state host]} (stub {:tokens 469000 :goal? true
                                    :transcript [(goal-line goal)]})
        job (compact/start! "%7" (assoc fast :host host
                                        :idle-timeout-ms 20000
                                        :between-turns? false))]
    (is (= [:failed :timeout] [(:status job) (:reason job)]))
    (is (= :wait-idle (get-in job [:error :during])))
    (is (= :settled-idle (get-in job [:error :waited-for])))
    (is (empty? (:sent @state)))))

(deftest a-verb-call-compacts-a-goal-pane-between-turns
  ;; Through `swarm tmux compact`, as the supervisor calls it: pane= names the
  ;; pane, force= skips the threshold, and the default proves rest by the
  ;; turn boundary because the /goal is active.
  (let [{:keys [state host]} (stub {:tokens 469000 :goal? true
                                   :transcript [(goal-line goal)]})
        r (binding [host/*host* host]
            (registry/dispatch {:command "compact" :pane "%7" :force true}))
        started (first (get-in r [:ok :started]))
        ;; The verb starts a real background job: wait for it, bounded. The
        ;; clock is stubbed, so the job makes its steps without real time.
        job (loop [n 200]
              (let [j (compact/job (:job-id started))]
                (if (or (#{:done :failed :skipped} (:status j)) (zero? n))
                  j
                  (do (Thread/sleep 10) (recur (dec n))))))]
    (is (nil? (:error r)) (pr-str r))
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (true? (:between-turns? job)) "auto picked the turn boundary from the active /goal")
    (is (= ["/compact" reseed (str "/goal " goal)] (:sent @state)))))

(deftest a-boundary-timeout-names-the-turn-boundary-it-waited-for
  ;; A goal pane whose rest never comes at all still fails, and the error says
  ;; turn-boundary, not settled-idle: a supervisor must be able to tell which
  ;; wait expired.
  (let [{:keys [state host]} (stub {:tokens 469000 :goal? true
                                    :transcript [(goal-line goal)]
                                    :rest-every 0})
        job (compact/start! "%7" (assoc fast :host host :idle-timeout-ms 20000))]
    (is (= [:failed :timeout] [(:status job) (:reason job)]))
    (is (= :turn-boundary (get-in job [:error :waited-for])))
    (is (= :wait-idle (get-in job [:error :during])))
    (is (empty? (:sent @state)))))

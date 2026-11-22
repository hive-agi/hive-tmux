(ns hive-tmux.claude.compact-busy-after-test
  "A compaction that worked is not a failed job. Jobs cc-04c4ee28 (%7) and
   cc-804a41e1 (%13), interrupt=true on a /goal pane (friction
   20261121201539-4a5836cb): :compacted {:by :ctx-drop} (463k -> 151k), the
   re-seed sent, then about 20 min :running and `failed timeout`, because
   the /goal kept the pane working and the job waited for a turn boundary
   that never came. cc-5ee44ff3 (%3) ended `done pane-busy-after-compact`
   on the same pane shape. Against the fake /goal pane port; no tmux, no
   clock, no with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.goal-pane :as gp]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def fast {:poll-ms 1000 :settle-ms 2000 :sync? true :interrupt true
           :idle-timeout-ms (* 20 60 1000) :interrupt-after-ms (* 2 60 1000)})

(def reseed compact/default-reseed)

(defn- compacted? [s] (boolean (some #{"/compact"} (:sent s))))

(defn- goal-survives
  "The /goal is back in force once the compaction ran: its Stop hook is armed."
  [s]
  (cond-> s (compacted? s) (assoc :goal? true)))

(defn- busy-after-reseed
  "Once the re-seed is in, the Stop hook keeps the pane working for good."
  [s]
  (cond-> (goal-survives s)
    (some #{reseed} (:sent s)) (assoc :busy? true :tool :ss-wait :busy-reads 1000000)))

(defn- busy-after-compact
  "The Stop hook restarts the next turn the moment the compaction ends."
  [s]
  (cond-> (goal-survives s)
    (and (compacted? s) (zero? (or (:compacting s) 0)))
    (assoc :busy? true :tool :ss-wait :busy-reads 1000000)))

(defn- run [tick]
  (let [{:keys [state host]} (gp/goal-pane {:tick tick :tokens 463000 :tokens-after 151000})]
    {:job (compact/start! "%9" (merge fast {:host host})) :pane @state}))

(deftest a-goal-that-keeps-the-pane-busy-after-the-reseed-is-done-not-failed
  (let [{:keys [job pane]} (run busy-after-reseed)]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (nil? (:error job)) "nothing failed: the wait after the compaction only advises")
    (is (true? (:compacted? job)))
    (is (= :ctx-drop (:compacted-by job)))
    (is (= 151000 (:tokens-after-compact job)))
    (is (= ["/compact" reseed] (:sent pane)) "nothing is typed into the working pane after the re-seed")
    (is (= :pane-busy-after-reseed (:reason job)))
    (is (= [(str "/goal " gp/goal-text)] (:unsent job)) "the supervisor is told what was not delivered")
    (is (nil? (:reseed-pending job)) "the re-seed went out")
    (is (true? (:goal-active-after? job)) "the goal is in force on the pane")
    (is (nil? (:goal-pending job)) "so it is not pending")
    (is (= :reseed (get-in job [:advisory :during])))
    (is (= :turn-boundary (get-in job [:advisory :waited-for])))))

(deftest a-goal-that-keeps-the-pane-busy-after-the-compact-is-done-with-the-reseed-pending
  (let [{:keys [job pane]} (run busy-after-compact)]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (= :pane-busy-after-compact (:reason job)))
    (is (nil? (:error job)))
    (is (= ["/compact"] (:sent pane)))
    (is (= [reseed (str "/goal " gp/goal-text)] (:unsent job)))
    (is (true? (:reseed-pending job)))))

(defn- busy-after-reseed-goal-cleared
  "The compaction cleared the goal, and the re-seed's turn never ends."
  [s]
  (cond-> s
    (some #{reseed} (:sent s)) (assoc :busy? true :tool :ss-wait :busy-reads 1000000)))

(deftest a-reseed-whose-turn-never-ends-times-out-into-done-with-the-goal-pending
  (let [{:keys [job pane]} (run busy-after-reseed-goal-cleared)]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (= :pane-busy-after-reseed (:reason job)))
    (is (nil? (:error job)))
    (is (= :timeout (get-in job [:advisory :reason])) "the timeout is kept, as advice")
    (is (= ["/compact" reseed] (:sent pane)))
    (is (true? (:goal-pending job)) "the goal was cleared and not restored")
    (is (= [(str "/goal " gp/goal-text)] (:unsent job)))))

(deftest a-job-that-never-compacted-still-fails-on-timeout
  (testing "the interrupt is never allowed: a write in flight"
    (let [{:keys [state host]} (gp/goal-pane {:tool :ship-land})
          job (compact/start! "%9" (merge fast {:host host}))]
      (is (= [:failed :timeout] [(:status job) (:reason job)]))
      (is (not (:compacted? job)))
      (is (empty? (:sent @state))))))

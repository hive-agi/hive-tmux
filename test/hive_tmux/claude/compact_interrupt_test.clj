(ns hive-tmux.claude.compact-interrupt-test
  "tmux compact interrupt=auto|true|false against the fake /goal pane port
   (`goal-pane`): a pane whose one turn never reaches a boundary. No tmux,
   no ~/.claude, no real clock, no with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.goal-pane :as gp]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.interrupt :as interrupt]
            [hive-tmux.claude.registry :as registry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def fast {:poll-ms 1000 :settle-ms 2000 :sync? true
           :idle-timeout-ms (* 20 60 1000) :interrupt-after-ms (* 2 60 1000)})

(defn- run [opts pane-opts]
  (let [{:keys [state host]} (gp/goal-pane pane-opts)]
    {:job (compact/start! "%9" (merge fast {:host host} opts)) :pane @state}))

;;; Pure

(deftest a-running-tool-is-read-off-the-screen
  (is (= :read-only (:kind (interrupt/running-tool (gp/screen {:busy? true :tool :ss-wait :tokens 1})))))
  (is (= :read-only (:kind (interrupt/running-tool (gp/screen {:busy? true :tool :sleep :tokens 1})))))
  (let [t (interrupt/running-tool (gp/screen {:busy? true :tool :ship-land :tokens 1}))]
    (is (= :write (:kind t)) "a call spread over two lines is read whole")
    (is (re-find #"ship land" (:tool t))))
  (is (nil? (interrupt/running-tool (gp/screen {:busy? true :tool nil :tokens 1}))) "thinking: no tool")
  (testing "markers match where a word starts: the `rm ` of `swarm (MCP)` is no write"
    (is (= :read-only (interrupt/tool-kind "hive - swarm (MCP)(command: \"ss wait\")")))
    (is (= :write (interrupt/tool-kind "Bash(rm -rf target)"))))
  (testing "a write wins a tie with a wait"
    (is (= :write (interrupt/tool-kind "git(command: \"ship land\", wait_s: 1500)"))))
  (testing "a newer call drawn under the running one is unknown"
    (is (= :unknown (:kind (interrupt/running-tool
                            (into (get gp/tools :ss-wait) ["● Bash(git push)" "❯ "])))))))

(deftest the-verdict-never-cuts-a-write-or-a-draft
  (let [busy {:busy? true :ctx-tokens 1}
        ro {:kind :read-only :tool "ss wait"}
        facts {:row busy :tool ro :waited-ms 600000 :goal-at-start? true}
        auto {:interrupt :auto :interrupt-after-ms 300000}]
    (is (:interrupt? (interrupt/verdict auto facts)))
    (is (:interrupt? (interrupt/verdict auto (assoc facts :tool nil))) "empty prompt, model thinking")
    (is (= :waiting (:hold (interrupt/verdict auto (assoc facts :waited-ms 1000)))))
    (is (= :no-goal (:hold (interrupt/verdict auto (assoc facts :goal-at-start? false)))))
    (is (= :unknown-tool (:hold (interrupt/verdict auto (assoc facts :tool {:kind :unknown})))))
    (is (= :write-in-flight (:hold (interrupt/verdict auto (assoc facts :tool {:kind :write})))))
    (is (= :prompt-holds-text (:hold (interrupt/verdict auto (assoc-in facts [:row :input] "half")))))
    (is (= :not-busy (:hold (interrupt/verdict auto (assoc facts :row {:busy? false})))))
    (is (= :interrupt-off (:hold (interrupt/verdict {:interrupt false} facts))))
    (testing "true drops the goal, wait and unknown gates, never the write gate"
      (let [t {:interrupt true}]
        (is (:interrupt? (interrupt/verdict t (assoc facts :goal-at-start? false :waited-ms 0
                                                     :tool {:kind :unknown}))))
        (is (= :write-in-flight (:hold (interrupt/verdict t (assoc facts :tool {:kind :write})))))
        (is (= :prompt-holds-text (:hold (interrupt/verdict t (assoc-in facts [:row :input] "x")))))))))

(deftest params-parse-to-the-three-modes
  (is (= [true false :auto :auto :auto true false]
         (mapv interrupt/mode-param [true false nil "auto" "bogus" "TRUE" "off"])))
  (is (= [:escape :force-send :force-send] (mapv interrupt/via-param ["escape" "force-send" nil]))))

;;; The job against the fake /goal pane

(deftest without-interrupt-the-job-times-out-at-wait-idle
  (let [{:keys [job pane]} (run {:interrupt false} {})]
    (is (= [:failed :timeout :wait-idle] [(:status job) (:reason job) (get-in job [:error :during])]))
    (is (empty? (:sent pane)))
    (is (empty? (:keys pane)) "not one key reached the working pane")))

(deftest auto-force-sends-compact-over-an-ss-wait-and-restores-the-goal
  (let [{:keys [job pane]} (run {} {})]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (= :force-send (:interrupted-via job)))
    (is (= ["/compact" "hive workflow catchup /caveman" (str "/goal " gp/goal-text)] (:sent pane))
        "compact, re-seed, then the goal /compact cleared")
    (is (= :ss-wait (:backgrounded pane)) "the running wait moved to the background, not killed")
    (is (not (:escaped pane)))
    (is (true? (:goal-active-after? job)))
    (is (some #{["C-x" "C-s"]} (:keys pane)))
    (is (>= (- (:clock pane) 0) (:interrupt-after-ms fast)) "it waited interrupt_after_ms first")))

(deftest auto-escape-stops-the-turn-then-compacts
  (let [{:keys [job pane]} (run {:interrupt-via :escape} {})]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (= :escape (:interrupted-via job)))
    (is (= 2 (count (filter #{["Escape"]} (:keys pane)))) "INSERT mode: the first Esc only leaves it")
    (is (= ["/compact" "hive workflow catchup /caveman" (str "/goal " gp/goal-text)] (:sent pane)))))

(deftest auto-never-interrupts-a-write
  (let [{:keys [job pane]} (run {} {:tool :ship-land})]
    (is (= [:failed :timeout] [(:status job) (:reason job)]))
    (is (= :write-in-flight (get-in job [:error :interrupt-hold :hold])) "the timeout names why it held")
    (is (empty? (:keys pane)))
    (is (nil? (:write-cut pane))))
  (testing "nor does true"
    (let [{:keys [job pane]} (run {:interrupt true :interrupt-via :escape} {:tool :ship-land})]
      (is (= :failed (:status job)))
      (is (empty? (:keys pane))))))

(deftest auto-never-types-over-a-draft
  (let [{:keys [job pane]} (run {} {:human "half a sentence"})]
    (is (= :failed (:status job)))
    (is (= :prompt-holds-text (get-in job [:error :interrupt-hold :hold])))
    (is (empty? (:keys pane)))))

(deftest auto-needs-a-goal-true-does-not
  (let [{:keys [job pane]} (run {:force? true} {:goal? false})]
    (is (= :failed (:status job)))
    (is (empty? (:keys pane))))
  (let [{:keys [job pane]} (run {:force? true :interrupt true} {:goal? false :tool nil})]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (= ["/compact" "hive workflow catchup /caveman"] (:sent pane)) "no goal to restore")))

(deftest an-interrupt-is-a-keystroke-the-job-can-no-longer-be-cancelled
  (let [{:keys [job]} (run {} {})]
    (is (= :interrupt (:typed-step job)))
    (is (= :already-typed (:refuse (compact/cancel! (:job-id job)))))))

;;; The verbs

(deftest the-compact-verb-takes-interrupt-and-cancel-is-a-verb
  ;; The verb starts a background job: a real 2 ms sleep with a frozen fake
  ;; clock keeps it holding (write in flight) until it is cancelled.
  (let [{:keys [state host]} (gp/goal-pane {:tool :ship-land})
        host (assoc host :sleep! (fn [_] (Thread/sleep 2)))]
    (binding [host/*host* host]
      (let [r (registry/dispatch {:command "compact" :pane "%9" :interrupt "auto"
                                  :interrupt_after_ms 1000 :interrupt_via "escape"})
            id (get-in r [:ok :started 0 :job-id])]
        (is (some? id) (pr-str r))
        (Thread/sleep 50)
        (testing "a stuck job is cancelled with `tmux cancel`, freeing the slot"
          (let [c (registry/dispatch {:command "cancel" :job_id id})]
            (is (= :cancelled (get-in c [:ok :status])) (pr-str c)))
          (is (= :tmux/already-cancelled (:error (registry/dispatch {:command "cancel" :job_id id}))))
          (is (= :tmux/no-job-id (:error (registry/dispatch {:command "cancel"})))))
        (is (empty? (:keys @state)) "the held job never typed")))))

(ns hive-tmux.claude.compact-queued-test
  "A message someone SUBMITTED while the turn ran waits in Claude Code's
   queue; it is not a draft. Pane %8, job cc-a49d0119 (friction
   20261121190008-0eb4cb6a): `tmux compact interrupt=true` failed
   :composer-mismatch, composer \"ct\", the operator's queued message. And
   the same pane's goal, set without a /goal entry, read as unreadable.
   Against the fake /goal pane port; no tmux, no clock, no with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.goal-pane :as gp]
            [hive-tmux.claude.panes :as panes]
            [hive-tmux.claude.screen :as screen]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def fast {:poll-ms 1000 :settle-ms 2000 :sync? true
           :idle-timeout-ms (* 20 60 1000) :interrupt-after-ms (* 2 60 1000)})

(defn- run [opts pane-opts]
  (let [{:keys [state host]} (gp/goal-pane pane-opts)]
    {:job (compact/start! "%9" (merge fast {:host host} opts)) :pane @state}))

;;; The screen

(def pane-8
  "Pane %8 as captured on 2026-11-21 (Claude Code 2.1.330), escapes kept."
  ["\u001b[38;5;246m●\u001b[39m \u001b[1mhive - swarm (MCP)\u001b[0m(command: \"ss wait\", parent: \"coordinator:15c7760e\", timeout_ms: 100000)"
   "\u001b[38;5;246m  ⎿ \u00a0Running…\u001b[39m"
   ""
   "\u001b[38;5;246m\u001b[48;5;237m❯ ct\u001b[39m"
   "\u001b[38;5;246m\u001b[49m  ⎿  Queued · ctrl+x ctrl+s to send now\u001b[39m"
   ""
   "\u001b[38;5;174m✢\u001b[39m \u001b[38;5;174mTomfoolering… \u001b[38;5;246m(58m 34s · ↓\u001b[39m \u001b[38;5;246m30.5k tokens)\u001b[39m"
   "\u001b[38;5;246m  ⎿ \u00a0Tip: Use /btw to ask a side question without interrupting Claude\u001b[39m"
   "                 \u001b[38;5;246m691125 tokens · \u001b[38;5;153m◎ /goal active (3h)\u001b[39m"
   "\u001b[38;5;244m────────────────────────────────────────\u001b[39m"
   "\u001b[38;5;246m❯\u00a0\u001b[2m\u001b[39mPress up to edit queued messages\u001b[0m"
   "\u001b[38;5;244m────────────────────────────────────────\u001b[39m"
   "\u001b[39m  \u001b[38;5;246m-- INSERT --\u001b[39m \u001b[38;5;220m⏵⏵ auto mode on\u001b[39m"])

(deftest a-queued-message-is-not-a-draft
  (let [s (screen/read-screen pane-8)]
    (is (nil? (:input s)) "nothing was typed at the prompt")
    (is (= ["ct"] (:queued s)))
    (is (= screen/queue-hint (:suggestion s)))
    (is (true? (:busy? s)))
    (is (= 691125 (:ctx-tokens s)))
    (is (true? (:goal-active? s))))
  (testing "a capture without escapes reads the same"
    (let [s (screen/read-screen (mapv screen/strip-ansi pane-8))]
      (is (nil? (:input s)))
      (is (= ["ct"] (:queued s)))))
  (testing "two queued messages, oldest first"
    (let [s (screen/read-screen (gp/screen {:tokens 5 :busy? true :tool :ss-wait :goal? true
                                            :queued ["ct" "also look at #30"]}))]
      (is (= ["ct" "also look at #30"] (:queued s)))
      (is (nil? (:input s)))))
  (testing "a draft is still a draft, queue or none"
    (is (= "half a thought" (:input (screen/read-screen (gp/screen {:tokens 5 :busy? true :human "half a thought"})))))
    (is (= "half a thought" (:input (screen/read-screen (gp/screen {:tokens 5 :busy? true :queued ["ct"]
                                                                    :human "half a thought"})))))
    (is (nil? (:queued (screen/read-screen (gp/screen {:tokens 5 :busy? true :human "x"})))))))

;;; The goal

(deftest the-goal-is-read-from-goal-status-when-no-goal-entry-exists
  (is (= "take on todo tasks to completion using hive swarm"
         (panes/goal-text ["{\"type\":\"x\"}" (gp/goal-status-line "take on todo tasks to completion using hive swarm")])))
  (testing "the newest entry wins, of either kind"
    (is (= "new" (panes/goal-text [(gp/goal-line "old") (gp/goal-status-line "new")])))
    (is (= "newer" (panes/goal-text [(gp/goal-status-line "old") (gp/goal-line "newer")]))))
  (testing "a met goal, or a cleared one, is none"
    (is (nil? (panes/goal-text [(gp/goal-status-line "done" true)])))
    (is (nil? (panes/goal-text [(gp/goal-status-line "x") (gp/goal-line "clear")])))))

;;; The job

(deftest the-queued-human-message-is-sent-first-then-compact
  (let [{:keys [job pane]} (run {:interrupt true} {:queued ["ct"] :recall-queued? true})]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (= ["ct"] (:human-delivered pane)) "the operator's message was delivered, not dropped")
    (is (= ["ct"] (:human-sent job)))
    (is (= ["ct" "/compact" "hive workflow catchup /caveman" (str "/goal " gp/goal-text)] (:sent pane))
        "the human's message first, then compact, re-seed, goal")
    (is (true? (:compacted? job)))
    (is (= [:human-sent :interrupt :compact]
           (filterv #{:human-sent :interrupt :compact} (map :step (:steps job)))))))

(deftest auto-flushes-the-queue-too
  (let [{:keys [job pane]} (run {} {:queued ["ct" "and #30"] :recall-queued? true})]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (= ["ct" "and #30"] (:human-delivered pane)))))

(deftest a-compact-already-queued-is-not-typed-twice
  (let [{:keys [job pane]} (run {:interrupt true} {:queued ["ct" "/compact"]})]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (= 1 (count (filter #{"/compact"} (:sent pane)))))
    (is (= ["ct"] (:human-delivered pane)))
    (is (not-any? #(= ["-l" "/compact"] %) (:keys pane)) "no second /compact typed")))

(deftest an-unsent-draft-still-blocks-the-job
  (let [{:keys [job pane]} (run {:interrupt true} {:queued ["ct"] :human "half a thought"})]
    (is (= :failed (:status job)))
    (is (= :prompt-holds-text (get-in job [:error :interrupt-hold :hold])))
    (is (empty? (:keys pane)) "not a key: neither the draft nor the queue was touched")
    (is (nil? (:human-delivered pane)))))

(deftest a-goal-set-without-a-goal-entry-is-restored
  (let [cond-text "take on todo tasks to completion using hive swarm"
        {:keys [job pane]} (run {:interrupt true}
                                {:queued ["ct"]
                                 :transcript ["{\"type\":\"x\"}" (gp/goal-status-line cond-text)]})]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (= (str "/goal " cond-text) (peek (:sent pane))))
    (is (nil? (:goal-unreadable job)))))

(deftest an-unreadable-goal-is-reported-and-the-pane-still-compacts
  (let [{:keys [job pane]} (run {:interrupt true} {:transcript ["{\"type\":\"x\"}"]})]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (true? (:compacted? job)))
    (is (re-find #"will NOT be restored" (:goal-unreadable job)))
    (is (some #(= :goal-unreadable (:step %)) (:steps job)))
    (is (not-any? #(.startsWith ^String % "/goal") (:sent pane)))))

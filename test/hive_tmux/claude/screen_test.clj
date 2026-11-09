(ns hive-tmux.claude.screen-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.screen :as screen]))

(def busy-screen
  ["● hive - swarm (MCP)(command: \"ss wait\", parent: \"coordinator:70283\", timeout_ms: 100000)"
   "✶ Percolating… (23m 49s · ↓ 27.2k tokens)"
   "  ⎿  Tip: Use /btw to ask a quick side question without interrupting Claude's current work"
   "                                                                       203674 tokens · ◎ /goal active (23m)"
   "──────────────────────────────────────────── caveman mode bootstrap ─"
   "❯ "
   "─────────────────────────────────────────────"
   "  -- INSERT -- ⏵⏵ auto mode on (shift+tab to cycle) · ← 24 agents"])

(def idle-screen
  ["● Done. Card closed."
   "✻ Worked for 3m 12s"
   "        41250 tokens · current: 2.1.319"
   "──────────────────── caveman skill hydration ─"
   "❯ "
   "─────────────────────"
   "  -- NORMAL -- ⏵⏵ auto mode on"])

(deftest busy-pane-reads-every-fact
  (let [s (screen/read-screen busy-screen)]
    (is (:claude? s))
    (is (= 203674 (:ctx-tokens s)) "the status-line count, not the spinner's 27.2k")
    (is (true? (:busy? s)))
    (is (= "Percolating… (23m 49s · ↓ 27.2k tokens)" (:activity s)))
    (is (true? (:goal-active? s)))
    (is (= "23m" (:goal-age s)))
    (is (= "caveman mode bootstrap" (:footer-name s)))
    (is (= :insert (:vim-mode s)))))

(deftest idle-pane-is-not-busy-and-has-no-goal
  (let [s (screen/read-screen idle-screen)]
    (is (= 41250 (:ctx-tokens s)))
    (is (false? (:busy? s)) "`Worked for` has no ellipsis: a finished turn, not a running one")
    (is (false? (:goal-active? s)))
    (is (= :normal (:vim-mode s)))
    (is (= "caveman skill hydration" (:footer-name s)))))

(deftest a-screen-without-prompt-is-unread-not-idle
  (let [s (screen/read-screen ["$ ls" "README.md"])]
    (is (false? (:claude? s)))
    (is (nil? (:busy? s)) "unknown, which a supervisor must not read as idle")
    (is (nil? (:ctx-tokens s)))))

(deftest text-typed-at-the-prompt-cannot-fake-a-status
  (let [s (screen/read-screen (conj (vec idle-screen) "❯ 999999 tokens · ◎ /goal active (1m)"))]
    (testing "facts are read above the LAST prompt only"
      (is (not= 999999 (:ctx-tokens s))))))

(deftest line-matchers
  (is (= 260803 (screen/ctx-tokens-of "   260803 tokens · ◎ /goal active (26m) · current: 2.1.319")))
  (is (nil? (screen/ctx-tokens-of "* Determining… (10m 52s · ↓ 37.3k tokens)")))
  (is (nil? (screen/ctx-tokens-of "about 12 tokens left in the sentence")))
  (is (= {:age "26m"} (screen/goal-of "260803 tokens · ◎ /goal active (26m) · x")))
  (is (= {:age nil} (screen/goal-of "◎ /goal active")))
  (is (nil? (screen/activity-of "✻ Worked for 3m")))
  (is (= "Hashing… (26m 48s · ↓ 29.8k tokens)"
         (screen/activity-of "· Hashing… (26m 48s · ↓ 29.8k tokens)")))
  (is (nil? (screen/footer-name-of "─────────────")))
  (is (= "caveman catchup trigger" (screen/title-name "✳ caveman catchup trigger")))
  (is (= "OC | Processing" (screen/title-name "OC | Processing"))))

(ns hive-tmux.claude.screen-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.screen :as screen]
            [clojure.string :as str]
            [clojure.java.io :as io]))

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

(deftest the-prompt-line-reports-pending-input
  (is (nil? (:input (screen/read-screen busy-screen))) "an empty prompt has no input")
  (is (= "hive workflow catchup /caveman"
         (:input (screen/read-screen (assoc busy-screen 5 "❯ hive workflow catchup /caveman  "))))))

(deftest an-idle-prompt-drawn-with-a-no-break-space-has-no-input
  ;; Captured from a live Claude Code 2.1.319 pane: bytes 342 235 257 302 240.
  (is (nil? (:input (screen/read-screen (assoc busy-screen 5 "❯ ")))))
  (is (= "typed" (:input (screen/read-screen (assoc busy-screen 5 "❯ typed  "))))))

(deftest goal-achieved-on-screen-without-an-active-goal-is-goal-ended
  (let [lines ["● All cards closed." "✔ Goal achieved" "✻ Worked for 9m"
               "   88000 tokens · current: 2.1.319" "──── peer ─" "❯ "]]
    (is (true? (:goal-ended? (screen/read-screen lines))))
    (is (false? (:goal-ended? (screen/read-screen (assoc lines 3 "   88000 tokens · ◎ /goal active (1m)"))))
        "an active goal means a new one started")))

(deftest body-strips-the-chrome-under-the-conversation
  (let [b (screen/body (into ["● Done. Card closed." "  PR #12 merged." ""] busy-screen) 10)]
    (is (= "● Done. Card closed." (first b)))
    (is (str/starts-with? (last b) "● hive - swarm (MCP)") "a tool call is conversation, kept")
    (is (not-any? #(str/includes? % "Percolating") b) "the spinner is chrome")
    (is (not-any? #(str/includes? % "tokens ·") b) "the status line is chrome")
    (is (not-any? #(str/includes? % "Tip:") b) "the tip is chrome")))

(deftest body-strips-live-chrome-drawn-with-no-break-spaces
  ;; Shapes captured from live Claude Code 2.1.319 panes on 2026-11-09.
  (let [lines ["● Five lings are running."
               ""
               "✶ Lollygagging… (11m 59s · ↓ 31.9k tokens)"
               "  ⎿  Tip: Use /btw to ask a quick side question"
               "   203674 tokens · ◎ /goal active (23m)"
               "──── caveman mode bootstrap ─"
               "❯ "]]
    (is (= ["● Five lings are running."] (screen/body lines 10))))
  (is (= ["● last reply: answered"]
         (screen/body ["● last reply: answered" "✻ Worked for 4s" "   151000 tokens" "──── fake peer ─" "❯ "] 10))
      "the finished-turn line is chrome")
  (is (= ["✻ Worked for nothing, a sentence"]
         (screen/body ["✻ Worked for nothing, a sentence" "❯ "] 10))
      "only a duration after `for` makes it chrome"))

;; Fixtures are `tmux capture-pane -e -p` of a live Claude Code 2.1.x pane
;; (%0, 2026-11-11), escapes kept. ghost-suggestion and typed-then-suggestion
;; replace only the prompt line with the shapes friction 20261110151501 saw:
;; `❯ ESC[2mff and core-reload ESC[0m` on %9, typed text followed by a dim tail.
(defn- fixture [name]
  (str/split-lines (slurp (io/resource (str "hive_tmux/claude/fixtures/" name ".ansi")))))

(deftest a-dim-suggestion-is-not-pending-input
  (let [s (screen/read-screen (fixture "ghost-suggestion"))]
    (is (:claude? s))
    (is (nil? (:input s)) "SGR-2 ghost text was typed by no one")
    (is (= "ff and core-reload" (:suggestion s)))
    (is (= 295483 (:ctx-tokens s)) "facts above the prompt still read through the escapes")
    (is (true? (:goal-active? s)))
    (is (= "caveman mode sync" (:footer-name s)))
    (is (= :insert (:vim-mode s)))))

(deftest an-idle-captured-prompt-has-neither-input-nor-suggestion
  (let [s (screen/read-screen (fixture "idle-prompt"))]
    (is (:claude? s))
    (is (nil? (:input s)))
    (is (nil? (:suggestion s)))
    (is (false? (:busy? s)) "`Churned for 25m 22s` is a finished turn")))

(deftest typed-text-before-a-dim-tail-is-input
  (let [s (screen/read-screen (fixture "typed-then-suggestion"))]
    (is (= "hotfix" (:input s)) "only the bright run was typed")
    (is (= "-reload magit, plain git ok" (:suggestion s)))))

(deftest sgr-parsing
  (testing "dim is set by 2 alone or inside a compound, cleared by 0, 22 or a bare m"
    (is (= [["a" false] ["b" true] ["c" false]]
           (screen/sgr-runs "a\u001b[2mb\u001b[0mc")))
    (is (= [["b" true] ["c" false]]
           (screen/sgr-runs "\u001b[38;5;246;2mb\u001b[22mc")))
    (is (= [["b" true] ["c" false]]
           (screen/sgr-runs "\u001b[2mb\u001b[mc")))
    (is (= [["x" false]] (screen/sgr-runs "\u001b[38;5;2mx"))
        "a 2 that is the colour index of 38;5;2 is not dim"))
  (testing "strip-ansi leaves plain text"
    (is (= "❯ ff" (screen/strip-ansi "\u001b[39m❯ \u001b[2mff\u001b[0m")))
    (is (= "plain" (screen/strip-ansi "plain")))))

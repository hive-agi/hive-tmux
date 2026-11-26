(ns hive-tmux.claude.goal-loop-screen-test
  "Reading the /goal Stop-hook loop off a pane (card 20261122170735-7c6f5ad3,
   scope 1). The recorded-*-2.1.330 fixtures are captures (`capture-pane -e`)
   of a scratch Claude Code 2.1.330 pane on a private tmux server, its /goal
   set to an unreachable condition so the Stop hook re-prompts every turn:
     recorded-stop-hook                 the spinner while Stop hooks run
     recorded-goal-continuing           the evaluator's `Goal not yet met`
                                        line, the next turn already running
     recorded-stop-hook-queued-compact  a /compact typed meanwhile, Queued
   Pure: lines in, facts out. No tmux, no clock."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.panes :as panes]
            [hive-tmux.claude.poll :as poll]
            [hive-tmux.claude.screen :as screen]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- fixture [name]
  (str/split-lines (slurp (io/resource (str "hive_tmux/claude/fixtures/" name ".ansi")))))

(def ^:private golden
  "Fixture -> the goal-loop facts `screen/read-screen` must read off it."
  {"recorded-stop-hook-2.1.330"
   {:busy? true :goal-active? true :goal-status {:active? true :age "42s"}
    :stop-hook-running? true :goal-continuing? false :ctx-tokens 50093 :input nil}

   "recorded-goal-continuing-2.1.330"
   {:busy? true :goal-active? true :goal-status {:active? true :age "44s"}
    :stop-hook-running? false :goal-continuing? true :ctx-tokens 50093 :input nil}

   "recorded-stop-hook-queued-compact-2.1.330"
   {:busy? true :goal-active? true :goal-status {:active? true :age "2m"}
    :stop-hook-running? true :goal-continuing? true :ctx-tokens 51497 :input nil
    :queued ["/compact"]}

   "recorded-busy-goal-2.1.328"
   {:busy? true :goal-active? true :goal-status {:active? true :age "1h"}
    :stop-hook-running? false :goal-continuing? false}

   "recorded-idle-2.1.328"
   {:busy? false :goal-status nil :stop-hook-running? false :goal-continuing? false}})

(deftest golden-screens-read-the-goal-loop
  (doseq [[name want] golden]
    (let [facts (screen/read-screen (fixture name))]
      (is (= want (select-keys facts (keys want))) name))))

(deftest the-stop-hook-mark-is-read-only-inside-a-spinner
  (is (true? (screen/stop-hook-line? "* Pouncing… (running Stop hooks… 1/2 · 42s · ↓ 1.5k tokens)")))
  (is (false? (screen/stop-hook-line? "✻ Pouncing… (43s · ↓ 1.5k tokens)")))
  (is (false? (screen/stop-hook-line? "  the docs say running Stop hooks happens at turn end"))
      "prose that names the mark is not the spinner"))

(deftest the-continuing-line-opens-with-its-glyph
  (is (true? (screen/goal-continuing-line? "◯ Goal not yet met… continuing")))
  (is (false? (screen/goal-continuing-line? "  Reason: Goal not yet met because the file is missing")))
  (is (false? (screen/goal-continuing-line? "Goal not yet met"))
      "no glyph: a line of conversation, not the evaluator's")
  (is (false? (screen/goal-continuing-line? "✘ Goal couldn't be achieved (6m · 15 turns · 9.1k tokens)"))))

(deftest a-continuing-line-without-an-active-goal-is-not-the-loop
  ;; Scrollback keeps the evaluator's line after the goal was cleared; with no
  ;; footer goal the screen is not a goal loop.
  (let [lines (mapv #(str/replace (screen/strip-ansi %) "◎ /goal active (44s)" "")
                    (fixture "recorded-goal-continuing-2.1.330"))
        facts (screen/read-screen lines)]
    (is (false? (:goal-active? facts)))
    (is (nil? (:goal-status facts)))
    (is (false? (:goal-continuing? facts)))))

(deftest pane-rows-carry-the-loop-facts-only-when-shown
  (let [pane {:pane "%1" :target "s:0.1" :cwd "/w" :title "" :tty "/dev/pts/1"}
        loop-row (panes/pane-row pane (screen/read-screen (fixture "recorded-stop-hook-2.1.330")) [])
        idle-row (panes/pane-row pane (screen/read-screen (fixture "recorded-idle-2.1.328")) [])]
    (is (= {:goal-status {:active? true :age "42s"} :stop-hook-running? true}
           (select-keys loop-row [:goal-status :stop-hook-running? :goal-continuing?])))
    (is (= {} (select-keys idle-row [:goal-status :stop-hook-running? :goal-continuing?]))
        "absent, not false: the pane-row contract of every other pane is unchanged")))

(deftest goal-looping-needs-an-active-goal-a-busy-pane-and-loop-evidence
  (testing "the recorded loop screens"
    (doseq [name ["recorded-stop-hook-2.1.330" "recorded-goal-continuing-2.1.330"
                  "recorded-stop-hook-queued-compact-2.1.330"]]
      (is (true? (poll/goal-looping? (screen/read-screen (fixture name)))) name)))
  (testing "a busy /goal pane with no loop evidence (a long tool call) is not looping"
    (is (false? (poll/goal-looping? (screen/read-screen (fixture "recorded-busy-goal-2.1.328"))))))
  (testing "rows"
    (is (false? (poll/goal-looping? nil)))
    (is (false? (poll/goal-looping? {:goal-active? true :busy? false :stop-hook-running? true})))
    (is (false? (poll/goal-looping? {:goal-active? false :busy? true :goal-continuing? true})))
    (is (true? (poll/goal-looping? {:goal-active? true :busy? true :goal-continuing? true})))))

(ns hive-tmux.claude.compact-goal-loop-e2e-test
  "e2e (card 20261122170735-7c6f5ad3): `compact!` through the REAL CLI host
   adapter, against a REAL tmux pane running a script that behaves like a
   Claude Code 2.1.330 pane held busy by a /goal Stop-hook loop
   (fixtures/fake_goal_claude.py, started as `exec -a claude`). The compact
   job must complete and the reseed must carry the goal text.

   Private tmux server (`tmux -L hive-e2e-<uuid>`, killed after) and a
   private temp claude home (sessions/<pid>.json, projects/<slug>/<sid>.jsonl);
   the operator's server and ~/.claude are never touched. Tagged ^:e2e: the
   default run skips it. Run: clojure -M:test --config-file tests-e2e.edn
   Requires tmux and python3."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.poll :as poll]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private goal "Ship every lane of the e2e plan, then report the PR numbers.")

(defn- tmp-dir [prefix]
  (str (java.nio.file.Files/createTempDirectory prefix (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- script-path []
  (.getAbsolutePath (io/file (io/resource "hive_tmux/claude/fixtures/fake_goal_claude.py"))))

(defn- lines-of [f] (if (.isFile (io/file f)) (str/split-lines (slurp f)) []))

(deftest ^:e2e a-goal-looping-tmux-pane-is-compacted-and-reseeded-with-its-goal
  (let [sock (str "hive-e2e-" (random-uuid))
        home (tmp-dir "hive-e2e-claude-")
        cwd (tmp-dir "hive-e2e-cwd-")
        sid "e2e00000-0000-4000-8000-000000000001"
        transcript (str home "/projects/" (host/project-slug cwd) "/" sid ".jsonl")
        tmux (fn [& args] (apply shell/sh "tmux" "-L" sock args))]
    (assert (str/starts-with? sock "hive-e2e-"))
    (io/make-parents transcript)
    (try
      (tmux "new-session" "-d" "-s" "e2e" "-x" "140" "-y" "40" "-c" cwd
            (str "exec -a claude python3 " (script-path) " " transcript " 470000 '" goal "'"))
      (Thread/sleep 1500)
      (let [[pane pid] (str/split (str/trim (:out (tmux "list-panes" "-a" "-F" "#{pane_id} #{pane_pid}"))) #" ")]
        ;; The session file Claude Code would write, so the CLI host joins it.
        ;; Its status is "idle": the script cannot rewrite it per turn, so the
        ;; screen's spinner is the busy signal this test exercises.
        (spit (doto (io/file home "sessions" (str pid ".json")) io/make-parents)
              (json/write-str {:pid (parse-long pid) :sessionId sid :name "coordinator e2e" :cwd cwd
                               :status "idle" :updatedAt 1}))
        (let [h (host/cli-host {:socket-name sock :claude-home home})
              row (poll/observe h pane)
              _ (is (true? (poll/goal-looping? row)) (pr-str (select-keys row [:busy? :goal-active? :stop-hook-running? :goal-continuing?])))
              job (compact/start! pane {:host h :sync? true :force? true :poll-ms 400 :settle-ms 800
                                        :idle-timeout-ms 60000 :step-timeout-ms 60000 :start-timeout-ms 20000})
              sent (lines-of (str transcript ".sent"))
              reseed (some #(when (str/starts-with? % "hive workflow catchup") %) sent)]
          (is (= :done (:status job)) (pr-str (dissoc job :steps)))
          (is (true? (:goal-loop? job)))
          (is (true? (:compacted? job)))
          (is (= ["/goal clear" "/compact"] (take 2 sent)) (pr-str sent))
          (is (some? reseed) (pr-str sent))
          (is (str/includes? (str reseed) goal) "the reseed carries the goal text")
          (is (= (str "/goal " goal) (last sent)) "the goal is set again")
          (is (true? (:started? job)) "its footer shows the goal active again")))
      (finally
        (tmux "kill-server")))))

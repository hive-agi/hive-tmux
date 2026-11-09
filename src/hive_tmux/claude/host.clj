(ns hive-tmux.claude.host
  "The host port the Claude pane verbs read and write through, and its one
   real adapter.

   Stratum: Port + Boundary. Everything that touches the tmux server, the
   Claude Code state directory (~/.claude) or the clock is a key of the port
   map, so `panes` and `compact` are tested with a stub map and no tmux.

   The adapter shells out to the tmux CLI rather than libtmux: the panes a
   supervisor watches are on whatever server the operator runs, not only the
   `hive` session the python bridge owns."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [hive-dsl.result :refer [guard]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def Host
  "The port. Each value is a fn:
     :tmux!        (argv) or (argv stdin) -> {:exit int :out str :err str}
     :sessions     () -> [session-map ...]  (sessions/<pid>.json, parsed, keys keywordized)
     :transcript   (cwd session-id) -> [line ...] or nil
     :sleep!       (ms) -> nil
     :now-ms       () -> epoch ms"
  [:map
   [:tmux! fn?] [:sessions fn?] [:transcript fn?] [:sleep! fn?] [:now-ms fn?]])

(defn claude-home
  "The Claude Code state directory: $CLAUDE_CONFIG_DIR, else user.home/.claude."
  []
  (or (not-empty (System/getenv "CLAUDE_CONFIG_DIR"))
      (str (System/getProperty "user.home") "/.claude")))

(defn project-slug
  "The projects/ directory name of CWD: every character that is not a letter
   or digit becomes `-` (\"/srv/a.b\" -> \"-srv-a-b\")."
  [cwd]
  (apply str (map #(if (Character/isLetterOrDigit ^char %) % \-) (str cwd))))

(defn- read-json [f]
  (guard Exception nil (json/read-str (slurp f) :key-fn keyword)))

(defn- session-files
  "Only <pid>.json. The <pid>.<hash>.key files beside them are credentials and
   are never opened."
  [dir]
  (->> (.listFiles (io/file dir))
       (filter #(let [n (.getName ^java.io.File %)]
                  (and (str/ends-with? n ".json")
                       (every? (fn [c] (Character/isDigit ^char c))
                               (subs n 0 (- (count n) 5))))))))

(defn- tmux-argv [socket-name argv]
  (into (if socket-name ["tmux" "-L" socket-name] ["tmux"]) argv))

(defn cli-host
  "The real adapter. OPTS: :socket-name (tmux -L), :claude-home."
  ([] (cli-host {}))
  ([{:keys [socket-name] :as opts}]
   (let [home (or (:claude-home opts) (claude-home))]
     {:tmux! (fn tmux!
               ([argv] (tmux! argv nil))
               ([argv stdin]
                (let [cmd (tmux-argv socket-name argv)]
                  (apply shell/sh (cond-> cmd stdin (into [:in stdin]))))))
      :sessions (fn []
                  (vec (keep read-json (session-files (str home "/sessions")))))
      :transcript (fn [cwd session-id]
                    (let [f (io/file home "projects" (project-slug cwd)
                                     (str session-id ".jsonl"))]
                      (when (.isFile f)
                        (with-open [r (io/reader f)]
                          (vec (line-seq r))))))
      :sleep! (fn [ms] (Thread/sleep (long ms)))
      :now-ms (fn [] (System/currentTimeMillis))})))

(def ^:dynamic *host*
  "The port the verbs use when the caller passes none. Read at call time, so a
   caller may rebind it; the default is the CLI adapter on the default server."
  nil)

(defn host
  "The bound port, or a fresh CLI adapter."
  []
  (or *host* (cli-host)))

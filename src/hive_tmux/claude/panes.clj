(ns hive-tmux.claude.panes
  "Claude Code panes as data: which tmux pane runs which Claude session, how
   full its context is, and whether it is working.

   Strata:
     Collect   list-raw / capture        tmux reads through the host port
     Promote   parse-panes / join-session / goal-text / pane-row   pure
     Boundary  panes / pane              one call that does all of it

   Three sources are joined, because no single one is enough:
     tmux list-panes            pane id, target, cwd, title
     sessions/<pid>.json        session id, name, busy|idle (Claude Code's own word)
     the pane's screen          context tokens, /goal active, spinner

   A bg session's process is not a child of the pane that shows it, so the
   join is by name: the footer rule (or the pane title) names the session, the
   same name ListAgents prints. cwd breaks a tie between equal names."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-dsl.result :refer [guard]]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.screen :as screen]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private fields
  [:pane :session :window :index :pid :command :cwd :title])

(def ^:private list-format
  (str/join "\t" ["#{pane_id}" "#{session_name}" "#{window_index}" "#{pane_index}"
                  "#{pane_pid}" "#{pane_current_command}" "#{pane_current_path}"
                  "#{pane_title}"]))

(defn- split-on
  "S split at every SEP (a string), keeping empty fields."
  [s sep]
  (loop [acc [] from 0]
    (if-let [i (str/index-of s sep from)]
      (recur (conj acc (subs s from i)) (+ i (count sep)))
      (conj acc (subs s from)))))

;;; Promote

(defn parse-panes
  "Rows of `tmux list-panes -a -F list-format` OUT, one map per pane."
  [out]
  (->> (split-on (str out) "\n")
       (remove str/blank?)
       (map #(zipmap fields (split-on % "\t")))
       (map #(assoc % :target (str (:session %) ":" (:window %) "." (:index %))))
       vec))

(defn join-session
  "The session of SESSIONS that NAME names, preferring one whose cwd is CWD.
   {:session map :ambiguous? bool}, or nil when NAME matches none."
  [sessions name cwd]
  (let [named (filter #(= name (:name %)) sessions)
        here (filter #(= cwd (:cwd %)) named)
        pool (if (seq here) here named)]
    (when (seq pool)
      {:session (last (sort-by :updatedAt pool))
       :ambiguous? (> (count pool) 1)})))

(defn- command-args [text]
  (let [open "<command-args>" close "</command-args>"]
    (when-let [i (str/index-of text open)]
      (when-let [j (str/index-of text close i)]
        (subs text (+ i (count open)) j)))))

(defn- content-text [content]
  (if (string? content)
    content
    (str/join "\n" (keep :text content))))

(defn goal-text
  "The text of the latest /goal LINES (a transcript, oldest first) set, or nil
   when the latest /goal cleared it or none was ever set. A raw line is only a
   candidate; the marker is checked again on the decoded content, because a
   JSON writer may spell `/` as `\\/`."
  [lines]
  (let [mark "<command-name>/goal</command-name>"
        candidate? #(or (str/includes? % mark)
                        (str/includes? % "<command-name>\\/goal<\\/command-name>"))
        decoded (fn [line]
                  (let [text (some-> (guard Exception nil (json/read-str line :key-fn keyword))
                                     :message :content content-text)]
                    (when (and text (str/includes? text mark)) text)))
        text (some decoded (filter candidate? (rseq (vec lines))))
        args (some-> text command-args str/trim)]
    (when-not (or (str/blank? args) (#{"clear" "off" "stop"} (str/lower-case args)))
      args)))

(defn pane-row
  "One pane's row from its tmux PANE map, its SCREEN facts and SESSIONS."
  [pane facts sessions]
  (let [name* (or (:footer-name facts) (screen/title-name (:title pane)))
        {:keys [session ambiguous?]} (join-session sessions name* (:cwd pane))
        status (some-> (:status session) keyword)]
    (cond-> {:pane (:pane pane)
             :target (:target pane)
             :name name*
             :cwd (or (:cwd session) (:cwd pane))
             :session-id (:sessionId session)
             :job-id (:jobId session)
             :pid (:pid session)
             :kind (:kind session)
             :status status
             :ctx-tokens (:ctx-tokens facts)
             :busy? (boolean (or (:busy? facts) (= :busy status)))
             :goal-active? (:goal-active? facts)
             :goal-age (:goal-age facts)
             :activity (:activity facts)
             :vim-mode (:vim-mode facts)
             :input (:input facts)}
      ambiguous? (assoc :ambiguous? true)
      (nil? session) (assoc :unjoined? true))))

;;; Collect

(defn list-raw
  "Every pane on the server, as `parse-panes` rows."
  [{:keys [tmux!]}]
  (let [{:keys [exit out err]} (tmux! ["list-panes" "-a" "-F" list-format])]
    (if (zero? exit)
      (parse-panes out)
      (throw (ex-info (str "tmux list-panes failed: " (str/trim (str err)))
                      {:type :tmux/list-failed :exit exit})))))

(defn capture
  "The last LINES screen lines of PANE-ID, trailing blanks trimmed."
  [{:keys [tmux!]} pane-id lines]
  (let [{:keys [exit out]} (tmux! ["capture-pane" "-p" "-t" pane-id "-S" (str (- lines))])]
    (when (zero? exit)
      (mapv str/trimr (split-on (str out) "\n")))))

(defn claude-pane?
  "Is tmux PANE running Claude Code? Its foreground command names it, or its
   process title is the Claude Code version (`2.1.319`)."
  [{:keys [command]}]
  (let [c (str command)]
    (or (= "claude" c)
        (and (seq c) (every? #(or (Character/isDigit ^char %) (= \. %)) c)))))

;;; Boundary

(defn panes
  "Every Claude Code pane as a row. OPTS:
     :include-goal?  also read each goal-active pane's transcript for :goal
     :lines          screen lines to read (default 40)"
  ([] (panes (host/host) {}))
  ([h] (panes h {}))
  ([h {:keys [include-goal? lines] :or {lines 40}}]
   (let [sessions ((:sessions h))]
     (->> (list-raw h)
          (filter claude-pane?)
          (mapv (fn [p]
                  (let [row (pane-row p (screen/read-screen (or (capture h (:pane p) lines) [])) sessions)]
                    (if (and include-goal? (:goal-active? row) (:session-id row))
                      (assoc row :goal (goal-text (or ((:transcript h) (:cwd row) (:session-id row)) [])))
                      row))))))))

(defn pane
  "The row of the pane REF names: a pane id (%3), a target (1:3.1), a session
   name or a session id. Nil when none matches."
  ([ref] (pane (host/host) ref {}))
  ([h ref] (pane h ref {}))
  ([h ref opts]
   (let [rows (panes h opts)]
     (or (some #(when (= ref (:pane %)) %) rows)
         (some #(when (= ref (:target %)) %) rows)
         (some #(when (= ref (:session-id %)) %) rows)
         (some #(when (= ref (:name %)) %) rows)))))

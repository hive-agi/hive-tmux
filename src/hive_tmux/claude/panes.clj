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
  [:pane :session :window :index :pid :command :cwd :title :tty])

(def ^:private list-format
  (str/join "\t" ["#{pane_id}" "#{session_name}" "#{window_index}" "#{pane_index}"
                  "#{pane_pid}" "#{pane_current_command}" "#{pane_current_path}"
                  "#{pane_title}" "#{pane_tty}"]))

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

(defn- goal-entry
  "What one transcript LINE says of the goal: {:args text} for a `/goal`
   command entry, {:condition text :met? bool} for Claude Code's own
   `goal_status` attachment, nil otherwise. A raw line is only a candidate;
   the marker is checked again on the decoded entry, because a JSON writer
   may spell `/` as `\\/`."
  [line]
  (let [mark "<command-name>/goal</command-name>"
        line (str line)]
    (when (or (str/includes? line mark)
              (str/includes? line "<command-name>\\/goal<\\/command-name>")
              (str/includes? line "goal_status"))
      (when-let [e (guard Exception nil (json/read-str line :key-fn keyword))]
        (let [att (:attachment e)
              text (some-> e :message :content content-text)]
          (cond
            (and (= "goal_status" (:type att)) (string? (:condition att)))
            {:condition (:condition att) :met? (true? (:met att))}

            (and text (str/includes? text mark))
            {:args (or (some-> text command-args str/trim) "")}))))))

(defn goal-text
  "The goal the latest goal entry of LINES (a transcript, oldest first) set,
   or nil when that entry cleared or met it, or there is none.

   Two entries say it. A `/goal <text>` command typed at the prompt, and the
   `goal_status` attachment Claude Code writes each time it checks the goal
   (`{\"type\":\"goal_status\",\"met\":false,\"condition\":<text>}`). A goal
   set some other way (pane %8, 2026-11-21: a /goal in a queued message, or
   one restored on resume) leaves no command entry, only the attachment, so
   the newest of either kind wins."
  [lines]
  (let [entry (some goal-entry (rseq (vec lines)))]
    (cond
      (nil? entry) nil
      (:condition entry) (when-not (:met? entry) (not-empty (str/trim (:condition entry))))
      :else (let [args (:args entry)]
              (when-not (or (str/blank? args) (#{"clear" "off" "stop"} (str/lower-case args)))
                args)))))

(defn last-assistant-text
  "The text of the newest assistant message in LINES (a transcript, oldest
   first) that says something: thinking and tool calls are skipped. Nil when
   there is none."
  [lines]
  (some (fn [line]
          (when (str/includes? line "\"assistant\"")
            (let [entry (guard Exception nil (json/read-str line :key-fn keyword))
                  content (get-in entry [:message :content])
                  text (when (= "assistant" (:type entry))
                         (if (string? content)
                           content
                           (->> content (filter #(= "text" (:type %))) (keep :text) (str/join "\n"))))]
              (not-empty (some-> text str/trim)))))
        (rseq (vec lines))))

(defn compact-boundaries
  "Every compaction LINES (a transcript, oldest first) records, oldest first:
   Claude Code writes one `{\"type\":\"system\",\"subtype\":\"compact_boundary\"}`
   line when a /compact (or an auto compaction) has finished, whatever the
   screen showed meanwhile. Each is {:pre-tokens :post-tokens :trigger}, a
   key nil when the line does not carry it."
  [lines]
  (into []
        (keep (fn [line]
                (when (str/includes? (str line) "compact_boundary")
                  (let [e (guard Exception nil (json/read-str line :key-fn keyword))]
                    (when (and (= "system" (:type e)) (= "compact_boundary" (:subtype e)))
                      (let [m (:compactMetadata e)]
                        {:pre-tokens (:preTokens m)
                         :post-tokens (:postTokens m)
                         :trigger (:trigger m)}))))))
        lines))

(defn pane-row
  "One pane's row from its tmux PANE map, its SCREEN facts and SESSIONS."
  [pane facts sessions]
  (let [title-name (screen/title-name (:title pane))
        names (distinct (remove nil? [(:footer-name facts) title-name]))
        [name* joined] (or (some (fn [name]
                                   (when-let [joined (join-session sessions name (:cwd pane))]
                                     [name joined]))
                                 names)
                           [(first names) nil])
        {:keys [session ambiguous?]} joined
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
             :goal-ended? (boolean (:goal-ended? facts))
             :goal-age (:goal-age facts)
             :activity (:activity facts)
             :vim-mode (:vim-mode facts)
             :input (:input facts)
             :suggestion (:suggestion facts)
             :tty (not-empty (:tty pane))}
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
  "The last LINES screen lines of PANE-ID, trailing blanks trimmed. Escapes are
   kept (`-e`): the dimmed prompt suggestion differs from typed text only by
   its SGR 2, so `screen/read-screen` needs them. Strip with
   `screen/strip-ansi` before showing a line."
  [{:keys [tmux!]} pane-id lines]
  (let [{:keys [exit out]} (tmux! ["capture-pane" "-p" "-e" "-t" pane-id "-S" (str (- lines))])]
    (when (zero? exit)
      (mapv str/trimr (split-on (str out) "\n")))))

(defn claude-pane?
  "Is tmux PANE running Claude Code? Its foreground command names it, or its
   process title is the Claude Code version (`2.1.319`)."
  [{:keys [command]}]
  (let [c (str command)]
    (or (= "claude" c)
        (and (seq c) (every? #(or (Character/isDigit ^char %) (= \. %)) c)))))

(defn codex-pane?
  "Is the pane's foreground command Codex? Titles are not identity evidence."
  [{:keys [command]}]
  (= "codex" command))

(defn- codex-row
  "Read-only terminal identity. Codex state is not inferred from Claude files."
  [pane]
  (assoc (select-keys pane [:pane :target :cwd :title :command])
         :pane-pid (:pid pane)
         :provider :codex
         :status :unknown
         :capabilities #{:read-screen}
         :unjoined? true))

;;; Boundary

(defn panes
  "Assistant panes as rows. OPTS:
     :provider       :claude (default), :codex or :all
     :include-goal?  read Claude goal-active transcripts for :goal
     :lines          Claude screen lines to read (default 40).
   Automation callers retain the Claude-only default. Codex inventory does
   not capture its screen or join it to Claude session files."
  ([] (panes (host/host) {}))
  ([h] (panes h {}))
  ([h {:keys [include-goal? lines provider] :or {lines 40 provider :claude}}]
   (when-not (#{:claude :codex :all} provider)
     (throw (ex-info "Unsupported pane provider" {:provider provider})))
   (let [raw (filterv (fn [p]
                       (or (and (#{:claude :all} provider) (claude-pane? p))
                           (and (#{:codex :all} provider) (codex-pane? p))))
                     (list-raw h))
         sessions (when (some claude-pane? raw) ((:sessions h)))]
     (mapv (fn [p]
             (if (codex-pane? p)
               (codex-row p)
               (let [row (assoc (pane-row p (screen/read-screen (or (capture h (:pane p) lines) [])) sessions)
                                :provider :claude
                                :capabilities #{:read-screen :say :compact :goal})]
                 (if (and include-goal? (:goal-active? row) (:session-id row))
                   (assoc row :goal (goal-text (or ((:transcript h) (:cwd row) (:session-id row)) [])))
                   row))))
           raw))))

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

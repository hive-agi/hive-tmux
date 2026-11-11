(ns hive-tmux.claude.registry
  "The `tmux` subdomain of the host's `swarm` tool: Claude Code panes as data
   for a supervisor, and compaction with re-seed and goal restore.

     swarm tmux panes     every Claude pane: pane, target, name, session id, cwd,
                          ctx-tokens, busy?, status, goal-active?, goal-age
     swarm tmux compact   compact one pane, or every pane over threshold
     swarm tmux job       one compaction job by id, or all of them"
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            [hive-addon.cli.response :as response]
            [hive-dsl.result :as r :refer [guard]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.panes :as panes]
            [hive-addon.registry.commands :as addon-cmds]
            [hive-tmux.claude.watch :as watch]
            [hive-tmux.claude.screen :as screen]
            [hive-tmux.claude.sense :as sense]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def host-tool "swarm")
(def subdomain "tmux")

(defn- tokens [s delims]
  (enumeration-seq (java.util.StringTokenizer. (str s) delims)))

(defn- as-list [x]
  (cond (nil? x) nil
        (string? x) (seq (remove str/blank? (map str/trim (tokens x ","))))
        :else (seq x)))

(defn- bool [x default]
  (cond (nil? x) default
        (string? x) (not (#{"false" "0" "no"} (str/lower-case x)))
        :else (boolean x)))

(defn- int-param [x default]
  (cond (nil? x) default
        (number? x) (long x)
        :else (or (parse-long (str x)) default)))

(defn- h-of [{:keys [socket_name]}]
  (if socket_name (host/cli-host {:socket-name socket_name}) (host/host)))

(defn- row-view [row]
  (into {} (remove (comp nil? val)) row))

(defn- refs-of [row]
  [(:pane row) (:target row) (:name row) (:session-id row)])

(defn panes-cmd [params]
  (let [rows (panes/panes (h-of params) {:include-goal? (bool (:include_goal params) false)})]
    (r/ok {:count (count rows)
           :panes (mapv row-view rows)})))

(defn- compact-opts [params h]
  (cond-> {:host h}
    (:threshold params) (assoc :threshold (int-param (:threshold params) nil))
    (contains? params :reseed) (assoc :reseed (str (:reseed params)))
    (some? (:restore_goal params)) (assoc :restore-goal? (bool (:restore_goal params) true))
    (:instructions params) (assoc :instructions (str (:instructions params)))
    (:goal params) (assoc :goal (str (:goal params)))
    (:goal_append params) (assoc :goal-append (str (:goal_append params)))
    (bool (:force params) false) (assoc :force? true)))

(defn- start-job!
  "Every job this subdomain starts reports its end to the pane watcher, which
   shouts it as a :sense/tmux-job when a watcher runs."
  [ref kind opts]
  (compact/start! ref (assoc opts :kind kind :on-end watch/job-ended!)))

(defn- resolve-one [h ref]
  (some #(when (some #{ref} (refs-of %)) %) (panes/panes h)))

(defn compact-cmd
  "pane given: compact those panes, whatever their size.
   pane omitted: every Claude pane over threshold, except those in exclude.
   dry_run: say what would be compacted, start nothing."
  [params]
  (let [h (h-of params)
        opts (compact-opts params h)
        threshold (or (:threshold opts) (:threshold compact/defaults))
        exclude (set (as-list (:exclude params)))
        rows (panes/panes h)
        refs (as-list (:pane params))
        targets (if refs
                  (keep (fn [ref] (some #(when (some #{ref} (refs-of %)) %) rows)) refs)
                  (filter #(and (not (some exclude (refs-of %))) (compact/due? % threshold)) rows))
        plan (mapv #(select-keys % [:pane :target :name :ctx-tokens :busy? :goal-active?]) targets)]
    (cond
      (and refs (empty? targets))
      (r/err :tmux/no-such-pane {:pane refs :known (mapv :name rows)})

      (bool (:dry_run params) false)
      (r/ok {:dry-run true :threshold threshold :would-compact plan})

      :else
      (r/ok {:threshold threshold
             :started (mapv #(start-job! (:pane %) :compact (cond-> opts refs (assoc :force? true)))
                            targets)
             :hint "Poll with `swarm tmux job job_id=<id>`; status goes queued -> running -> done | skipped | failed."}))))

(defn job-cmd [{:keys [job_id]}]
  (if job_id
    (if-let [j (compact/job job_id)]
      (r/ok j)
      (r/err :tmux/no-such-job {:job_id job_id}))
    (r/ok {:jobs (mapv #(dissoc % :steps) (compact/jobs))})))

(def ^:private pane-hint "pane=<%id|1:2.1|name|session-id>")

(defn goal-cmd
  "Arm or replace a pane's /goal without compacting it."
  [params]
  (let [h (h-of params)
        ref (:pane params)]
    (cond
      (str/blank? (str ref)) (r/err :tmux/no-pane {:hint pane-hint})
      (not (or (:goal params) (:goal_append params))) (r/err :tmux/no-goal {:hint "goal=<text> or goal_append=<text>"})
      (nil? (resolve-one h ref)) (r/err :tmux/no-such-pane {:pane ref})
      :else (r/ok (start-job! ref :goal
                              (cond-> {:host h}
                                (:goal params) (assoc :goal (str (:goal params)))
                                (:goal_append params) (assoc :goal-append (str (:goal_append params)))
                                (some? (:wait_idle params)) (assoc :wait-idle? (bool (:wait_idle params) true))))))))

(defn say-cmd
  "Submit one prompt to a pane."
  [params]
  (let [h (h-of params)
        ref (:pane params)
        text (some-> (:text params) str str/trim not-empty)]
    (cond
      (str/blank? (str ref)) (r/err :tmux/no-pane {:hint pane-hint})
      (nil? text) (r/err :tmux/no-text {:hint "text=<prompt>"})
      (nil? (resolve-one h ref)) (r/err :tmux/no-such-pane {:pane ref})
      :else (r/ok (start-job! ref :say {:host h :text text
                                        :wait-idle? (bool (:wait_idle params) true)})))))

(defn read-cmd
  "A pane's newest assistant message (from its transcript) and its screen body."
  [params]
  (let [h (h-of params)
        ref (:pane params)
        n (int-param (:lines params) 40)
        row (when-not (str/blank? (str ref)) (resolve-one h ref))]
    (if-not row
      (r/err :tmux/no-such-pane {:pane ref :hint pane-hint})
      (let [lines (or (panes/capture h (:pane row) (+ n 30)) [])
            transcript (when (:session-id row) ((:transcript h) (:cwd row) (:session-id row)))]
        (r/ok (cond-> {:pane (:pane row) :name (:name row) :busy? (:busy? row)
                       :goal-active? (:goal-active? row) :goal-ended? (:goal-ended? row)
                       :screen (if (bool (:strip_chrome params) true)
                                 (screen/body lines n)
                                 (mapv screen/strip-ansi (take-last n lines)))}
                (and transcript (bool (:last_assistant params) true))
                (assoc :last-assistant (panes/last-assistant-text transcript))))))))

(defn watch-start-cmd [params]
  (let [parent (some-> (or (:parent params) (:agent_id params)) str str/trim not-empty)]
    (if-not parent
      (r/err :tmux/no-parent {:hint "parent=<your agent id>: the shouts are addressed to it, and `ss wait` hears its own parent by default"})
      (r/ok (watch/start! (cond-> {:parent parent :host (h-of params)}
                            (:threshold params) (assoc :threshold (int-param (:threshold params) nil))
                            (:interval_ms params) (assoc :interval-ms (int-param (:interval_ms params) nil))
                            (:exclude params) (assoc :exclude (vec (as-list (:exclude params))))))))))

(defn watch-stop-cmd [_] (r/ok (or (watch/stop!) {:running? false})))

(defn watch-status-cmd [_] (r/ok (watch/status)))

(def subcommands
  {"panes"   {:fn panes-cmd
              :doc "Every Claude Code pane as data: pane, target, name, session-id, cwd, ctx-tokens, busy?, status, goal-active?, goal-age, activity, input (text typed at the prompt) and suggestion (Claude Code's dimmed ghost text there, typed by no one; never relay it as the owner's words). include_goal=true adds the /goal text."
              :params [:include_goal :socket_name]}
   "compact" {:fn compact-cmd
              :doc "Wait idle, capture the /goal, send /compact, re-seed (default \"hive workflow catchup /caveman\"), restore the goal. pane=<ref,...> compacts those panes whatever their size; no pane = every pane over threshold (default 400000) minus exclude. Background job; returns job ids."
              :params [:pane :threshold :reseed :restore_goal :goal :goal_append :instructions :exclude :force :dry_run :socket_name]}
   "goal"    {:fn goal-cmd
              :doc "Arm or replace a pane's /goal WITHOUT compacting: goal=<text> replaces, goal_append=<text> extends the active one. Waits for a settled idle (wait_idle=false: skip a busy pane); text at the prompt stops it. Background job; job ends with goal-active-after?."
              :params [:pane :goal :goal_append :wait_idle :socket_name]}
   "say"     {:fn say-cmd
              :doc "Submit one prompt (text=) to a pane. Waits for a settled idle unless wait_idle=false; refuses (skipped, pending-human-input) when the prompt line holds text. Job is done once the pane starts on it; read the answer later with `tmux read`."
              :params [:pane :text :wait_idle :socket_name]}
   "read"    {:fn read-cmd
              :doc "A pane's newest assistant message from its transcript (last_assistant, default true) and its last N screen lines (lines, default 40) without the status bar, spinner, tip and rules (strip_chrome, default true)."
              :params [:pane :lines :strip_chrome :last_assistant :socket_name]}
   "watch start" {:fn watch-start-cmd
                  :doc "Watch every Claude pane server-side and shout changes to parent=<agent id> as sixth-sense senses: pane-idle, goal-ended, context-high (threshold, default 400000) and tmux-job (a compact|say|goal job ended). Then `swarm ss wait` instead of polling. interval_ms default 5000; exclude=<refs> (your own pane)."
                  :params [:parent :threshold :interval_ms :exclude :socket_name]}
   "watch stop"   {:fn watch-stop-cmd :doc "Stop the pane watcher." :params []}
   "watch status" {:fn watch-status-cmd :doc "The pane watcher: running?, parent, ticks, events fired per kind, last error." :params []}
   "job"     {:fn job-cmd
              :doc "One compaction job by job_id with its steps, or every job without."
              :params [:job_id]}})

(defn normalize-command [command]
  (let [ws (tokens (str/lower-case (str/trim (str command))) " \t\n")]
    (if (seq ws) (str/join " " ws) "help")))

(defn strip-subdomain-prefix [command]
  (let [ws (tokens command " \t\n")]
    (if (= subdomain (some-> (first ws) str/lower-case))
      (str/join " " (rest ws))
      (str command))))

(defn help [_]
  (r/ok {:subdomain (str host-tool " " subdomain)
         :commands (into (sorted-map)
                         (map (fn [[k {:keys [doc params]}]] [k {:doc doc :params params}]))
                         subcommands)}))

(defn dispatch [{:keys [command] :as params}]
  (let [c (normalize-command command)]
    (if (= "help" c)
      (help params)
      (if-let [{handler :fn} (get subcommands c)]
        (try (handler params)
             (catch Exception e
               (r/err :tmux/handler-threw {:command c :cause (.getMessage e)})))
        (r/err :tmux/unknown-command {:command c :known (vec (sort (keys subcommands)))})))))

(defn ->response [result]
  (if (r/err? result)
    (response/error (pr-str result))
    (response/text (with-out-str (pp/pprint (:ok result))))))

(defn dispatch-subdomain [params]
  (->response (dispatch (update params :command strip-subdomain-prefix))))

(def contributions
  {subdomain {:handler dispatch-subdomain
              :description (str "Claude Code panes in tmux, for a supervisor: "
                                (str/join ", " (sort (keys subcommands)))
                                ". Pane state as data and compaction that re-seeds and restores the /goal.")}})

(def schema-ext
  {"pane"         {:type "string" :description "[tmux compact] Pane ref(s), comma-separated: %3, 1:3.1, a session name or session id. Omit to sweep every pane over threshold."}
   "threshold"    {:type "integer" :description "[tmux compact] Context tokens at or above which a pane is compacted (default 400000)."}
   "reseed"       {:type "string" :description "[tmux compact] Prompt sent after /compact (default \"hive workflow catchup /caveman\"; empty string sends none)."}
   "restore_goal" {:type "boolean" :description "[tmux compact] Re-send the pane's /goal after the re-seed (default true)."}
   "instructions" {:type "string" :description "[tmux compact] Text after /compact, as Claude Code's compaction instructions."}
   "exclude"      {:type "string" :description "[tmux compact] Pane refs a sweep skips, comma-separated (e.g. your own pane)."}
   "force"        {:type "boolean" :description "[tmux compact] Compact even under threshold."}
   "dry_run"      {:type "boolean" :description "[tmux compact] List what would be compacted; start nothing."}
   "include_goal" {:type "boolean" :description "[tmux panes] Also read each goal-active pane's /goal text."}
   "socket_name"  {:type "string" :description "[tmux panes|compact] tmux -L socket; default server when omitted."}
   "goal"         {:type "string" :description "[tmux compact] Goal to set after the re-seed, replacing the pane's own /goal."}
   "goal_append"  {:type "string" :description "[tmux compact] Text appended to the goal that is set (the captured one, or `goal`)."}
   "text"         {:type "string" :description "[tmux say] The prompt to submit."}
   "wait_idle"    {:type "boolean" :description "[tmux say|goal] Wait for a settled idle first (default true); false skips a busy pane."}
   "lines"        {:type "integer" :description "[tmux read] Screen lines to return (default 40)."}
   "strip_chrome" {:type "boolean" :description "[tmux read] Drop the status bar, spinner, tip and rules (default true)."}
   "last_assistant" {:type "boolean" :description "[tmux read] Include the newest assistant message from the transcript (default true)."}
   "interval_ms"  {:type "integer" :description "[tmux watch start] Tick interval (default 5000)."}
   "job_id"       {:type "string" :description "[tmux job] Compaction job id from tmux compact."}})


(def addon-key
  "Ownership key for contributions, so retraction removes only ours."
  :hive-tmux)

(defn contribute!
  "Graft `swarm tmux ...` onto the host and register the pane senses with
   sixth-sense. The verbs need only the tmux CLI, not libtmux, so the addon
   contributes them before its preflight runs."
  []
  (guard Exception nil (sense/register!))
  (guard Exception nil (addon-cmds/contribute! host-tool addon-key contributions)))

(defn retract!
  "Withdraw `swarm tmux ...`, stop the pane watcher, unregister the senses."
  []
  (guard Exception nil (watch/stop!))
  (guard Exception nil (sense/unregister!))
  (guard Exception nil (addon-cmds/retract! host-tool addon-key)))

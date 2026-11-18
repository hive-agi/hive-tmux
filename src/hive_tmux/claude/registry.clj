(ns hive-tmux.claude.registry
  "The `tmux` subdomain of the host's `swarm` tool: Claude and Codex discovery,
   with Claude compaction, re-seed and goal restore.

     swarm tmux panes     assistant panes with provider and capabilities;
                          Claude: pane, target, name, session id, cwd,
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

(defn- between-turns-param
  "How `compact` proves a pane is at rest: `auto` (the default) reads a
   /goal-active pane at its single turn boundary, `true` forces that, `false`
   forces the stricter settled idle. Anything else is `auto`, never a silent
   `true`: guessing wrong would type into a working turn."
  [x]
  (let [s (str/lower-case (str/trim (str x)))]
    (cond (contains? #{"true" "1" "yes" "on"} s) true
          (contains? #{"false" "0" "no" "off"} s) false
          :else :auto)))

(defn- h-of [{:keys [socket_name]}]
  (if socket_name (host/cli-host {:socket-name socket_name}) (host/host)))

(defn- row-view [row]
  (into {} (remove (comp nil? val)) row))

(defn- refs-of [row]
  [(:pane row) (:target row) (:name row) (:session-id row)])

(defn panes-cmd [params]
  (let [given (:pane_provider params)
        provider (cond (nil? given) :all
                       (keyword? given) given
                       (string? given) (keyword given)
                       :else nil)]
    (if-not (#{:claude :codex :all} provider)
      (r/err :tmux/unsupported-provider {:provider given :supported [:all :claude :codex]})
      (let [rows (panes/panes (h-of params) {:provider provider
                                           :include-goal? (bool (:include_goal params) false)})]
        (r/ok {:count (count rows)
               :panes (mapv row-view rows)})))))

(defn- compact-opts [params h]
  (cond-> {:host h}
    (:threshold params) (assoc :threshold (int-param (:threshold params) nil))
    (contains? params :reseed) (assoc :reseed (str (:reseed params)))
    (some? (:restore_goal params)) (assoc :restore-goal? (bool (:restore_goal params) true))
    (:instructions params) (assoc :instructions (str (:instructions params)))
    (:goal params) (assoc :goal (str (:goal params)))
    (:goal_append params) (assoc :goal-append (str (:goal_append params)))
    (some? (:input_timeout_ms params)) (assoc :input-timeout-ms (int-param (:input_timeout_ms params) nil))
    (some? (:between_turns params)) (assoc :between-turns? (between-turns-param (:between_turns params)))
    (bool (:force params) false) (assoc :force? true)))

(defn- start-job!
  "Every job this subdomain starts reports its end to the pane watcher, which
   shouts it as a :sense/tmux-job when a watcher runs."
  [ref kind opts]
  (compact/start! ref (assoc opts :kind kind :on-end watch/job-ended!)))

(defn- resolve-one [h ref]
  (some #(when (some #{ref} (refs-of %)) %) (panes/panes h {:provider :all})))

(defn caller-pid
  "The pid in a `coordinator:<pid>` caller id (the host stamps it on every call
   as `_caller_id`), or nil. It is the pid of the caller's own Claude Code
   process, which is the :pid of the session its pane row joins."
  [caller-id]
  (let [prefix "coordinator:"]
    (when (and (string? caller-id) (str/starts-with? caller-id prefix))
      (let [digits (subs caller-id (count prefix))]
        (when (and (<= 1 (count digits) 10) (every? #(Character/isDigit ^char %) digits))
          (let [pid (parse-long digits)] (when (pos? pid) pid)))))))

(defn own-pane?
  "Does ROW show the session of the caller whose pid is PID?"
  [row pid]
  (boolean (and pid (number? (:pid row)) (= pid (long (:pid row))))))

(def self-compact
  "Where the caller compacts its own pane instead."
  {:use "swarm steer self input=/compact then=<reseed> dry_run=false"
   :why (str "a compact job waits for the pane to be at rest, and your own pane is working on this very"
             " turn until it ends. A /goal pane is read at its turn boundary, but your own boundary is the"
             " Stop hook this run is inside, so a job could only wait here: steer self types /compact into"
             " your composer and the re-seed once the window reset.")})

(defn compact-cmd
  "pane given: compact those panes, whatever their size.
   pane omitted: every Claude pane over threshold, except those in exclude.
   dry_run: say what would be compacted, start nothing.
   Only Claude panes compact: an explicit selection holding a pane without
   the :compact capability (e.g. Codex) starts no job.
   The caller's own pane (`_caller_id`) is never compacted here: named, the
   call is refused at once; in a sweep it is left out and reported, both
   pointing at `swarm steer self` (see `self-compact`)."
  [params]
  (let [h (h-of params)
        opts (compact-opts params h)
        threshold (or (:threshold opts) (:threshold compact/defaults))
        exclude (set (as-list (:exclude params)))
        rows (panes/panes h {:provider :all})
        refs (as-list (:pane params))
        me (caller-pid (:_caller_id params))
        matched (if refs
                  (keep (fn [ref] (some #(when (some #{ref} (refs-of %)) %) rows)) refs)
                  (filter #(and (= :claude (:provider %))
                                (not (some exclude (refs-of %)))
                                (compact/due? % threshold)) rows))
        own (into [] (comp (filter #(own-pane? % me)) (distinct)) matched)
        own-view (when (seq own)
                   (assoc self-compact :own-pane (mapv #(select-keys % [:pane :name :ctx-tokens :goal-active?]) own)))
        targets (remove #(own-pane? % me) matched)
        unsupported (filterv #(not (contains? (:capabilities %) :compact)) targets)
        plan (mapv #(select-keys % [:pane :target :name :provider :ctx-tokens :busy? :goal-active?]) targets)]
    (cond
      (and refs (empty? matched))
      (r/err :tmux/no-such-pane {:pane refs :known (mapv #(or (:name %) (:pane %)) rows)})

      (and refs (seq own))
      (r/err :tmux/own-pane own-view)

      (seq unsupported)
      (r/err :tmux/unsupported-provider {:panes (mapv #(select-keys % [:pane :provider :capabilities]) unsupported)
                                        :command :compact})

      (bool (:dry_run params) false)
      (r/ok (cond-> {:dry-run true :threshold threshold :would-compact plan}
              own-view (assoc :skipped-own-pane own-view)))

      :else
      (r/ok (cond-> {:threshold threshold
                     :started (mapv #(start-job! (:pane %) :compact (cond-> opts refs (assoc :force? true)))
                                    targets)
                     :hint "Poll with `swarm tmux job job_id=<id>`; status goes queued -> running -> done | skipped | failed."}
              own-view (assoc :skipped-own-pane own-view))))))

(defn job-cmd [{:keys [job_id]}]
  (if job_id
    (if-let [j (compact/job job_id)]
      (r/ok j)
      (r/err :tmux/no-such-job {:job_id job_id}))
    (r/ok {:jobs (mapv #(dissoc % :steps) (compact/jobs))})))

(def ^:private pane-hint "pane=<%id|1:2.1|name|session-id>")

(defn goal-cmd
  "Arm or replace a supported pane's /goal without compacting it."
  [params]
  (let [h (h-of params)
        ref (:pane params)
        row (when-not (str/blank? (str ref)) (resolve-one h ref))]
    (cond
      (str/blank? (str ref)) (r/err :tmux/no-pane {:hint pane-hint})
      (not (or (:goal params) (:goal_append params))) (r/err :tmux/no-goal {:hint "goal=<text> or goal_append=<text>"})
      (nil? row) (r/err :tmux/no-such-pane {:pane ref})
      (not (contains? (:capabilities row) :goal))
      (r/err :tmux/unsupported-provider {:pane ref :provider (:provider row) :command :goal
                                        :capabilities (:capabilities row)})
      :else (r/ok (start-job! ref :goal
                              (cond-> {:host h}
                                (:goal params) (assoc :goal (str (:goal params)))
                                (:goal_append params) (assoc :goal-append (str (:goal_append params)))
                                (some? (:wait_idle params)) (assoc :wait-idle? (bool (:wait_idle params) true))
                                (some? (:input_timeout_ms params)) (assoc :input-timeout-ms (int-param (:input_timeout_ms params) nil))))))))

(defn say-cmd
  "Submit one prompt to a pane with a tested input contract."
  [params]
  (let [h (h-of params)
        ref (:pane params)
        text (some-> (:text params) str str/trim not-empty)
        row (when-not (str/blank? (str ref)) (resolve-one h ref))]
    (cond
      (str/blank? (str ref)) (r/err :tmux/no-pane {:hint pane-hint})
      (nil? text) (r/err :tmux/no-text {:hint "text=<prompt>"})
      (nil? row) (r/err :tmux/no-such-pane {:pane ref})
      (not (contains? (:capabilities row) :say))
      (r/err :tmux/unsupported-provider {:pane ref :provider (:provider row) :command :say
                                        :capabilities (:capabilities row)})
      :else (r/ok (start-job! ref :say {:host h :text text
                                        :wait-idle? (bool (:wait_idle params) true)})))))

(defn read-cmd
  "Read a terminal screen. Claude panes can also return transcript text."
  [params]
  (let [h (h-of params)
        ref (:pane params)
        n (int-param (:lines params) 40)
        row (when-not (str/blank? (str ref)) (resolve-one h ref))]
    (if-not row
      (r/err :tmux/no-such-pane {:pane ref :hint pane-hint})
      (let [lines (or (panes/capture h (:pane row) (+ n 30)) [])
            claude? (= :claude (:provider row))
            transcript (when (and claude? (:session-id row))
                         ((:transcript h) (:cwd row) (:session-id row)))]
        (r/ok (cond-> {:pane (:pane row) :provider (:provider row)
                       :status (:status row) :capabilities (:capabilities row)
                       :screen (if (and claude? (bool (:strip_chrome params) true))
                                 (screen/body lines n)
                                 (mapv screen/strip-ansi (take-last n lines)))}
                claude? (assoc :name (:name row) :busy? (:busy? row)
                               :goal-active? (:goal-active? row) :goal-ended? (:goal-ended? row))
                (not claude?) (assoc :title (:title row) :screen-format :terminal)
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
              :doc "Claude Code and Codex panes as data with provider and capabilities. pane_provider=all (default), claude or codex. Claude rows include session, context, busy/input and optional goal facts; Codex rows expose terminal identity with status unknown and read-screen capability only. Titles do not identify a Codex task. include_goal=true reads Claude goals."
              :params [:pane_provider :include_goal :socket_name]}
   "compact" {:fn compact-cmd
              :doc "Wait for rest, capture the /goal, send /compact, re-seed (default \"hive workflow catchup /caveman\"), restore the goal. pane=<ref,...> compacts those panes whatever their size; no pane = every pane over threshold (default 400000) minus exclude. Rest is a settled idle, except on a pane whose /goal is active: a /goal Stop hook restarts the next turn within a poll, so there the job reads the single turn boundary between turns (between_turns=auto, the default; true forces it, false forces the settled idle). Text someone left at a prompt is waited out up to input_timeout_ms (default 600000), never typed over; the job shows it under :waiting-on. Your OWN pane is refused (a sweep leaves it out): compact yourself with `swarm steer self input=/compact then=<reseed> dry_run=false`. Background job; returns job ids."
              :params [:pane :threshold :reseed :restore_goal :goal :goal_append :instructions :exclude :force :dry_run :input_timeout_ms :between_turns :socket_name]}
   "goal"    {:fn goal-cmd
              :doc "Arm or replace a pane's /goal WITHOUT compacting: goal=<text> replaces, goal_append=<text> extends the active one. Waits for a settled idle (wait_idle=false: skip a busy pane); text at the prompt is waited out up to input_timeout_ms (not with wait_idle=false), then stops it. Background job; job ends with goal-active-after?."
              :params [:pane :goal :goal_append :wait_idle :input_timeout_ms :socket_name]}
   "say"     {:fn say-cmd
              :doc "Submit one prompt (text=) to a pane. Waits for a settled idle unless wait_idle=false; refuses (skipped, pending-human-input) when the prompt line holds text. Job is done once the pane starts on it; read the answer later with `tmux read`."
              :params [:pane :text :wait_idle :socket_name]}
   "read"    {:fn read-cmd
              :doc "Read a Claude or Codex pane's last N screen lines (lines, default 40). Claude may include transcript last_assistant and strip_chrome (both default true). Codex returns terminal text without Claude parsing, transcript attribution or inferred busy/input state."
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
              :description (str "Claude Code and Codex pane discovery in tmux, with Claude supervision: "
                                (str/join ", " (sort (keys subcommands)))
                                ". Pane state as data and compaction that re-seeds and restores the /goal.")}})

(def schema-ext
  {"pane_provider" {:type "string" :enum ["all" "claude" "codex"] :description "[tmux panes] Provider inventory filter (default all); independent of the model provider parameter."}
   "pane"         {:type "string" :description "[tmux compact] Pane ref(s), comma-separated: %3, 1:3.1, a session name or session id. Omit to sweep every pane over threshold."}
   "threshold"    {:type "integer" :description "[tmux compact] Context tokens at or above which a pane is compacted (default 400000)."}
   "reseed"       {:type "string" :description "[tmux compact] Prompt sent after /compact (default \"hive workflow catchup /caveman\"; empty string sends none)."}
   "restore_goal" {:type "boolean" :description "[tmux compact] Re-send the pane's /goal after the re-seed (default true)."}
   "instructions" {:type "string" :description "[tmux compact] Text after /compact, as Claude Code's compaction instructions."}
   "exclude"      {:type "string" :description "[tmux compact] Pane refs a sweep skips, comma-separated (e.g. your own pane)."}
   "force"        {:type "boolean" :description "[tmux compact] Compact even under threshold."}
   "dry_run"      {:type "boolean" :description "[tmux compact] List what would be compacted; start nothing."}
   "input_timeout_ms" {:type "integer" :description "[tmux compact|goal] How long text someone left at the prompt is waited out before the job skips as pending-human-input (default 600000; 0 skips at once)."}
   "between_turns" {:type "string" :description "[tmux compact] How rest is proven: auto (default) reads a /goal-active pane at its single turn boundary and every other pane at a settled idle; true forces the turn boundary, false forces the settled idle."}
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

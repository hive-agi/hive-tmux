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
            [hive-addon.registry.commands :as addon-cmds]))

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
    (bool (:force params) false) (assoc :force? true)))

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
             :started (mapv #(compact/start! (:pane %) (cond-> opts refs (assoc :force? true)))
                            targets)
             :hint "Poll with `swarm tmux job job_id=<id>`; status goes queued -> running -> done | skipped | failed."}))))

(defn job-cmd [{:keys [job_id]}]
  (if job_id
    (if-let [j (compact/job job_id)]
      (r/ok j)
      (r/err :tmux/no-such-job {:job_id job_id}))
    (r/ok {:jobs (mapv #(dissoc % :steps) (compact/jobs))})))

(def subcommands
  {"panes"   {:fn panes-cmd
              :doc "Every Claude Code pane as data: pane, target, name, session-id, cwd, ctx-tokens, busy?, status, goal-active?, goal-age, activity. include_goal=true adds the /goal text."
              :params [:include_goal :socket_name]}
   "compact" {:fn compact-cmd
              :doc "Wait idle, capture the /goal, send /compact, re-seed (default \"hive workflow catchup /caveman\"), restore the goal. pane=<ref,...> compacts those panes whatever their size; no pane = every pane over threshold (default 400000) minus exclude. Background job; returns job ids."
              :params [:pane :threshold :reseed :restore_goal :instructions :exclude :force :dry_run :socket_name]}
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
   "job_id"       {:type "string" :description "[tmux job] Compaction job id from tmux compact."}})

(def addon-key
  "Ownership key for contributions, so retraction removes only ours."
  :hive-tmux)

(defn contribute!
  "Graft `swarm tmux ...` onto the host. The verbs need only the tmux CLI, not
   libtmux, so the addon contributes them before its preflight runs."
  []
  (guard Exception nil (addon-cmds/contribute! host-tool addon-key contributions)))

(defn retract!
  "Withdraw `swarm tmux ...`."
  []
  (guard Exception nil (addon-cmds/retract! host-tool addon-key)))

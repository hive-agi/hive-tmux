(ns hive-tmux.claude.watch
  "Watch Claude Code panes server-side and shout each change, so the agent
   that supervises them hears it through sixth-sense (`swarm ss wait`)
   instead of polling tmux.

   Strata:
     Promote   step            pure: previous pane states + new rows -> states + events
     Boundary  start! / stop!  one watcher thread per process, shouts through the host

   Events (see hive-tmux.claude.sense for the sense classes):
     :pane-idle     a pane seen working is idle on two ticks in a row
     :goal-ended    goal-active? went true -> false, or `Goal achieved` appeared
     :context-high  ctx-tokens went from under the threshold to at or over it
     :tmux-job      a compact | say | goal job ended (via `job-ended!`)

   The first tick only seeds: a pane already idle when the watch starts is not
   news. Every shout is addressed to :parent, the supervisor's agent id, which
   is who `ss wait` hears by default."
  (:require [clojure.string :as str]
            [hive-dsl.result :refer [guard]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.panes :as panes]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def defaults {:interval-ms 5000 :threshold 400000})

;;; Promote

(defn- pane-step
  "One pane: its previous state PREV (nil on first sight) and ROW now ->
   [state events]. A pane must be seen working before its idle is news, and
   one working spell yields one :pane-idle."
  [prev row {:keys [threshold]}]
  (let [seed? (nil? prev)
        idle (compact/idle? row)
        streak (if idle (inc (:idle-streak prev 0)) 0)
        worked (boolean (or (:worked? prev) (:busy? row)))
        idle-event? (and (not seed?) worked (= 2 streak))
        goal-ended? (and (not seed?)
                         (or (and (:goal-active? prev) (false? (:goal-active? row))
                                  ;; an unreadable screen says false too; a count proves it was read
                                  (some? (:ctx-tokens row)))
                             (and (:goal-ended? row) (not (:goal-ended? prev)))))
        ctx (:ctx-tokens row)
        high? (boolean (and (not seed?) ctx (>= ctx threshold)
                            (some-> (:ctx-tokens prev) (< threshold))))]
    [{:idle-streak streak
      :worked? (if idle-event? false worked)
      :goal-active? (:goal-active? row)
      :goal-ended? (:goal-ended? row)
      :ctx-tokens (or ctx (:ctx-tokens prev))}
     (cond-> []
       idle-event? (conj [:pane-idle row])
       goal-ended? (conj [:goal-ended row])
       high? (conj [:context-high row]))]))

(defn step
  "All panes: STATES (pane id -> state) and ROWS now -> {:states :events}.
   EXCLUDE (a set of refs) drops panes before they are tracked. A pane that
   vanished is forgotten."
  [states rows {:keys [exclude] :as opts}]
  (let [rows (remove #(some (set exclude) [(:pane %) (:target %) (:name %) (:session-id %)]) rows)]
    (reduce (fn [acc row]
              (let [[st evs] (pane-step (get states (:pane row)) row opts)]
                (-> acc
                    (assoc-in [:states (:pane row)] st)
                    (update :events into evs))))
            {:states {} :events []}
            rows)))

(defn row-data
  "The shout data of an event about ROW."
  [row]
  (into {} (remove (comp nil? val))
        {:tmux-pane (:pane row) :tmux-target (:target row) :tmux-name (:name row)
         :tmux-session-id (:session-id row) :tmux-cwd (:cwd row)
         :tmux-ctx-tokens (:ctx-tokens row) :tmux-goal-active (:goal-active? row)}))

(defn- message [event row]
  (case event
    :pane-idle (str (:name row) " (" (:target row) ") is idle"
                    (if (:goal-active? row) ", goal still active" ", no goal active"))
    :goal-ended (str (:name row) " (" (:target row) ") goal ended")
    :context-high (str (:name row) " (" (:target row) ") context at " (:ctx-tokens row) " tokens")))

;;; Boundary

(def shout-sym 'hive-mcp.hivemind.messaging/shout!)

(defonce ^:private watcher (atom nil))

(defn- shout! [{:keys [parent shout-fn]} event data]
  (when-let [f (or shout-fn (guard Throwable nil (requiring-resolve shout-sym)))]
    (guard Throwable nil
      (f (str "tmux:" (or (:tmux-name data) "watch")) event
         (cond-> (assoc data :directory (:tmux-cwd data))
           parent (assoc :parent-id parent))))))

(defn tick!
  "One pass: read the panes, step, shout every event. Returns the events."
  [w]
  (let [{:keys [host opts states]} @w
        rows (panes/panes host)
        {next-states :states events :events} (step states rows opts)]
    (swap! w assoc :states next-states :last-tick (System/currentTimeMillis)
           :ticks (inc (:ticks @w 0)))
    (doseq [[event row] events]
      (shout! opts event (assoc (row-data row) :message (message event row)))
      (swap! w update-in [:fired event] (fnil inc 0)))
    events))

(defn running? [] (boolean (some-> @watcher deref :running?)))

(defn stop!
  "Stop the watcher. Returns its last status, or nil when none ran."
  []
  (when-let [w @watcher]
    (swap! w assoc :running? false)
    (reset! watcher nil)
    (dissoc @w :host :states :opts)))

(defn start!
  "Start (or restart) the watcher. OPTS: :parent (agent id the shouts are
   addressed to; required), :threshold, :interval-ms, :exclude (refs),
   :host and :shout-fn (tests)."
  [{:keys [parent] :as opts}]
  (when (str/blank? (str parent))
    (throw (ex-info "watch needs parent: the agent id that should hear the senses" {:type :tmux/no-parent})))
  (stop!)
  (let [opts (merge defaults opts)
        w (atom {:running? true :opts opts :host (or (:host opts) (host/host))
                 :states {} :started-at (System/currentTimeMillis) :ticks 0 :fired {}})]
    (reset! watcher w)
    (future
      (while (:running? @w)
        (try (tick! w)
             (catch Throwable t
               (swap! w assoc :last-error (.getMessage t))
               (log/warn "[claude-watch] tick failed" (.getMessage t))))
        (Thread/sleep (long (:interval-ms opts)))))
    (dissoc @w :host :states)))

(defn status []
  (if-let [w @watcher]
    (-> @w (dissoc :host) (update :states count)
        (update :opts dissoc :host :shout-fn) (assoc :running? true))
    {:running? false}))

(defn job-ended!
  "Shout a finished compact | say | goal JOB to the running watcher's parent.
   A no-op when no watcher runs: nobody asked to hear it."
  [job]
  (when-let [w @watcher]
    (let [opts (:opts @w)]
      (shout! opts :tmux-job
              (into {} (remove (comp nil? val))
                    {:message (str (name (:kind job :compact)) " " (:job-id job) " on " (:name job) ": "
                                   (name (:status job)) (some->> (:reason job) name (str " ")))
                     :tmux-pane (:pane job) :tmux-name (:name job)
                     :tmux-job-id (:job-id job) :tmux-job-kind (some-> (:kind job) name)
                     :tmux-job-status (some-> (:status job) name)
                     :tmux-job-reason (some-> (:reason job) name)})))))

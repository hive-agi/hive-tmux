(ns hive-tmux.claude.idle-capacity
  "S1 idle-capacity: the lazy-coordinator sense (card 20261122160543-4df00c21).

   Coordinators never sit back (axiom 20261003154522-36473755). A coordinator
   pane that has been at rest for more than :idle-ms (10 min), with no
   working ling under it in the sixth-sense pulse while its board still holds
   open todo or doing cards, is spare capacity. This namespace says so:

     :idle-capacity {:tmux-coordinator id :tmux-idle-ms n
                     :tmux-live-lings n :tmux-open-cards n}

   It never fires for a pane a human is busy with: a pane whose prompt holds
   text (:input) or whose session is :waiting on a dialog is not idle
   (`compact/human-guard`, the S3 human-draft guard), and the idle clock
   restarts there.

   A coordinator is matched by its Claude Code SESSION ID, never by pane id:
   tmux reuses pane ids, so a new session in an old pane starts a fresh clock.
   Its coordinator ids are the ones bb-mcp stamps (`coordinator:<first 8 hex of
   the session id>`, or `coordinator:<pid>` on an older launcher).

   Strata:
     Promote   coordinator-ids / quiet? / working? / step   pure over the ports
     Port      {:pulse :open-cards :now-ms}                 see `Ports`
     Boundary  default-ports                                soft-resolved hosts

   One firing per idle spell: the pane must work again (or a human touch it)
   before it can fire again. Pulse and board are read only for a pane already
   past the threshold, and at most once per :recheck-ms while it stays there."
  (:require [clojure.string :as str]
            [hive-dsl.result :refer [guard]]
            [hive-tmux.claude.compact :as compact]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def axiom-id "20261003154522-36473755")

(def defaults
  {:idle-ms (* 10 60 1000)
   :recheck-ms 60000})

(def Ports
  "The ports. Each value is a fn:
     :pulse       ([coordinator-id ...]) -> [{\"agent\" \"state\" ...} ...], the
                  live lings whose parent is one of the ids; nil when the
                  pulse cannot be read (then nothing fires: zero lings is
                  not proven)
     :open-cards  (pane-row) -> {:todo n :doing n}, the board of the pane's
                  project; nil when it cannot be read
     :now-ms      () -> epoch ms"
  [:map [:pulse fn?] [:open-cards fn?] [:now-ms {:optional true} fn?]])

;;; Promote

(defn coordinator-ids
  "The coordinator ids the session of ROW answers to: `coordinator:<first 8
   hex of its session id>` (bb-mcp's token, scripts/session-id.sh), then
   `coordinator:<pid>`. Empty without a session id: an unjoined pane is
   nobody's coordinator."
  [row]
  (let [sid (some-> (:session-id row) str str/trim not-empty)
        token (when sid (re-find #"^[0-9a-f]{8}" sid))]
    (if-not sid
      []
      (vec (distinct (remove nil? [(when token (str "coordinator:" token))
                                   (when (:pid row) (str "coordinator:" (:pid row)))]))))))

(defn quiet?
  "Is ROW's pane at rest with nobody's hand on it? Idle (`compact/idle?`) and
   no human guard (`compact/human-guard`: no draft at the prompt, no open
   dialog, not :waiting)."
  [row]
  (boolean (and (compact/idle? row) (nil? (compact/human-guard row)))))

(def not-working-states
  "Pulse states of a ling that is alive but does no work for its coordinator:
   it waits on it (ask, blocked) or has stopped making progress."
  #{"ask" "blocked" "error" "truncated" "context-death" "aborting" "completed"})

(defn ling-beat?
  "Is pulse row BEAT a ling? The pane watcher's own shouts (agent
   `tmux:<pane name>`) beat the pulse too, under the watcher's parent; they
   are panes, not lings."
  [beat]
  (not (str/starts-with? (str (get beat "agent")) "tmux:")))

(defn working?
  "Is pulse row BEAT a ling at work?"
  [beat]
  (and (ling-beat? beat)
       (not (contains? not-working-states (str (get beat "state"))))))

(defn open-count
  "Open cards of a board count map {:todo n :doing n}: todo plus doing."
  [counts]
  (+ (long (or (:todo counts) 0)) (long (or (:doing counts) 0))))

(defn check
  "The verdict on coordinator ROW, past the idle threshold, through PORTS:
     {:fire {...}}             nothing works under it and its board is open
     {:busy-lings n|:unknown}  lings are working, or the pulse is unreadable
     {:board counts}           the board holds no open card, or is unreadable"
  [{:keys [pulse open-cards]} row]
  (let [ids (coordinator-ids row)
        beats (guard Throwable nil (pulse ids))
        lings (when beats
                (->> beats (filter ling-beat?)
                     (map (juxt #(get % "agent") identity)) (into {}) vals))
        working (filter working? lings)]
    (cond
      (nil? beats) {:busy-lings :unknown}
      (seq working) {:busy-lings (count working)}
      :else
      (let [counts (guard Throwable nil (open-cards row))]
        (if (and counts (pos? (open-count counts)))
          {:fire {:coordinator (first ids) :coordinator-ids ids
                  :live-lings (count lings)
                  :open-cards (open-count counts) :counts counts}}
          {:board counts})))))

(defn- session-step
  "One coordinator session: previous state PREV (nil on first sight), ROW now
   -> [state event-or-nil]."
  [ports prev row now {:keys [idle-ms recheck-ms]}]
  (if-not (quiet? row)
    [{} nil]
    (let [since (or (:idle-since prev) now)
          idled (- now since)
          st (assoc prev :idle-since since)]
      (cond
        (or (:fired? prev) (<= idled idle-ms)) [st nil]
        (and (:checked-at prev) (< (- now (:checked-at prev)) recheck-ms)) [st nil]
        :else
        (let [{:keys [fire]} (check ports row)]
          (if fire
            [(assoc st :fired? true :checked-at now)
             [:idle-capacity (assoc row :idle-capacity (assoc fire :idle-ms idled))]]
            [(assoc st :checked-at now) nil]))))))

(defn step
  "All coordinator panes: STATES (session id -> state) and ROWS now ->
   {:states :events}. Only Claude panes joined to a session count, and they
   are keyed by session id, so a reused pane id is a new coordinator. A
   session no longer on screen is forgotten. OPTS :exclude (refs) drops panes."
  [ports states rows now opts]
  (let [opts (merge defaults opts)
        exclude (set (:exclude opts))
        rows (->> rows
                  (filter #(and (:session-id %) (not= :codex (:provider %))))
                  (remove #(some exclude [(:pane %) (:target %) (:name %) (:session-id %)])))]
    (reduce (fn [acc row]
              (let [sid (:session-id row)
                    [st ev] (session-step ports (get states sid) row now opts)]
                (cond-> (assoc-in acc [:states sid] st)
                  ev (update :events conj ev))))
            {:states {} :events []}
            rows)))

(defn event-data
  "The shout data an :idle-capacity event adds to its pane's row data."
  [row]
  (let [{:keys [coordinator idle-ms live-lings open-cards counts]} (:idle-capacity row)]
    (into {} (remove (comp nil? val))
          {:tmux-coordinator coordinator
           :tmux-idle-ms idle-ms
           :tmux-live-lings live-lings
           :tmux-open-cards open-cards
           :tmux-open-todo (:todo counts)
           :tmux-open-doing (:doing counts)})))

(defn message [row]
  (let [{:keys [coordinator idle-ms live-lings open-cards counts]} (:idle-capacity row)]
    (str (:name row) " (" (:target row) ", " coordinator ") idle " (quot (long idle-ms) 60000)
         " min with no working ling (" live-lings " live) and " open-cards " open cards ("
         (:todo counts 0) " todo, " (:doing counts 0) " doing): coordinators never sit back"
         " (axiom " axiom-id ")")))

;;; Boundary

(def pulse-sym 'hive-agent.sixth-sense.api/pulse)
(def board-summary-sym 'hive-mcp.tools.kanban.catchup-block/gather-kanban-summary)
(def project-id-sym 'hive-mcp.tools.memory.scope/get-current-project-id)

(defn- soft-resolve [sym]
  (guard Throwable nil (requiring-resolve sym)))

(defn pulse-port
  "A :pulse port over sixth-sense's `pulse` (resolved by RESOLVE-FN): the live
   beats under each coordinator id, read through a receptor on that parent.
   Answers nil (unreadable, so nothing fires) when sixth-sense is absent."
  [resolve-fn]
  (fn [ids]
    (when-let [pulse (resolve-fn pulse-sym)]
      (vec (mapcat #(pulse {:receptor {:parent %}}) ids)))))

(defn open-cards-port
  "An :open-cards port over the host's kanban summary of the project the
   pane's cwd belongs to. nil off-host, for a pane outside any project, or
   when the board cannot be read."
  [resolve-fn]
  (fn [row]
    (let [summary (resolve-fn board-summary-sym)
          project-id (resolve-fn project-id-sym)
          pid (when (and project-id (:cwd row)) (project-id (:cwd row)))]
      (when (and summary pid (not= "global" pid))
        (when-let [counts (:counts (summary pid))]
          {:todo (:todo counts 0) :doing (:inprogress counts 0)})))))

(defn default-ports
  "The ports a running watcher uses when it is given none."
  ([] (default-ports soft-resolve))
  ([resolve-fn]
   {:pulse (pulse-port resolve-fn)
    :open-cards (open-cards-port resolve-fn)
    :now-ms (fn [] (System/currentTimeMillis))}))

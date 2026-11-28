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
     :pane-waiting  the session status went to :waiting (a permission or other
                    human prompt); once per waiting spell
     :tmux-job      a compact | say | goal job ended (via `job-ended!`)
     :compact-ready the pane may be compacted (`hive-tmux.claude.ready`): over
                    the threshold, idle, empty prompt, no dialog, and its
                    coordinator WRAPPED or updated its hand-off after the
                    crossing; once per spell over the threshold, per session.
                    With :auto-compact? the watcher, as the ONE compactor
                    (a host-wide lease), silences the wrapped coordinator's
                    wakers and compacts it, re-seeding with its hand-off.
     :idle-capacity a coordinator session idle > 10 min, no working ling under
                    it, open cards on its board (`hive-tmux.claude.idle-capacity`)

   The first tick only seeds: a pane already idle when the watch starts is not
   news. Every shout is addressed to :parent, the supervisor's agent id, which
   is who `ss wait` hears by default."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-dsl.result :refer [guard]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.idle-capacity :as idle-capacity]
            [hive-tmux.claude.panes :as panes]
            [hive-tmux.claude.ready :as ready]
            [taoensso.timbre :as log]
            [hive-tmux.claude.caller :as caller]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def defaults {:interval-ms 5000 :threshold 400000 :evidence-interval-ms 30000})

;;; Promote

(defn- crossed-at
  "When the context of ROW crossed THRESHOLD upward, as this watcher saw it:
   NOW on the crossing (or on first sight of a pane already over it, since
   nothing earlier is known), PREV's time while it stays over, nil under it."
  [prev row threshold now]
  (let [ctx (:ctx-tokens row)]
    (cond (nil? ctx) (:crossed-at prev)
          (< ctx threshold) nil
          (:crossed-at prev) (:crossed-at prev)
          :else now)))

(defn- pane-step
  "One pane: its previous state PREV (nil on first sight) and ROW now ->
   [state events]. A pane must be seen working before its idle is news, and
   one working spell yields one :pane-idle. A pane id that now shows another
   session is a new pane: pane ids are reused, and nothing seen of the old
   session may fire for the new one.
   :compact-ready fires once per spell over the threshold, when
   `ready/verdict` holds on ROW with EVIDENCE (pane id -> evidence, read by
   the boundary for that session)."
  [prev row {:keys [threshold now evidence]}]
  (let [prev (when-not (and prev (:session-id prev) (:session-id row)
                            (not= (:session-id prev) (:session-id row)))
               prev)
        seed? (nil? prev)
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
                            (some-> (:ctx-tokens prev) (< threshold))))
        waiting (= :waiting (:status row))
        waiting? (and (not seed?) waiting (not (:waiting? prev)))
        crossed (crossed-at prev row threshold now)
        spell? (and crossed (= crossed (:crossed-at prev)))
        fired? (and spell? (:ready-fired? prev))
        v (when (and crossed (not fired?))
            (ready/verdict row threshold crossed (get evidence (:pane row))))
        ready? (boolean (:ready? v))]
    [{:idle-streak streak
      :worked? (if idle-event? false worked)
      :goal-active? (:goal-active? row)
      :goal-ended? (:goal-ended? row)
      :waiting? waiting
      :session-id (or (:session-id row) (:session-id prev))
      :ctx-tokens (or ctx (:ctx-tokens prev))
      :crossed-at crossed
      :ready-fired? (boolean (or fired? ready?))
      :silenced? (boolean (and spell? (:silenced? prev)))}
     (cond-> []
       idle-event? (conj [:pane-idle row])
       goal-ended? (conj [:goal-ended row])
       high? (conj [:context-high row])
       waiting? (conj [:pane-waiting row])
       ready? (conj [:compact-ready row (assoc (:evidence v) :crossed-at crossed)]))]))

(defn step
  "All panes: STATES (pane id -> state) and ROWS now -> {:states :events}.
   EXCLUDE (a set of refs) drops panes before they are tracked. A pane that
   vanished is forgotten. OPTS :now (epoch ms) and :evidence (pane id ->
   evidence) feed :compact-ready."
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
         :tmux-ctx-tokens (:ctx-tokens row) :tmux-goal-active (:goal-active? row)
         :tmux-status (some-> (:status row) name)}))

(defn ready-data
  "The shout data of a :compact-ready about ROW with its EVIDENCE."
  [row evidence]
  (into (row-data row) (remove (comp nil? val))
        {:tmux-ready-by (some-> (:by evidence) name)
         :tmux-handoff-id (:handoff-id evidence)
         :tmux-coordinator (first (ready/agent-ids row))}))

(defn- message [event row]
  (case event
    :pane-idle (str (:name row) " (" (:target row) ") is idle"
                    (if (:goal-active? row) ", goal still active" ", no goal active"))
    :goal-ended (str (:name row) " (" (:target row) ") goal ended")
    :context-high (str (:name row) " (" (:target row) ") context at " (:ctx-tokens row) " tokens")
    :pane-waiting (str (:name row) " (" (:target row) ") is waiting on a human (permission or prompt)")
    :compact-ready (str (:name row) " (" (:target row) ", session " (:session-id row) ") is compact-ready at "
                        (:ctx-tokens row) " tokens: idle, empty prompt, no dialog, and its coordinator wrapped")))

(defn evidence-wanted
  "The ROWS (not EXCLUDEd) whose coordinator evidence the boundary should
   read now: every cheap compact-ready condition holds (`ready/candidate?`),
   or, under auto-compact, the pane is over the threshold and not yet
   silenced (a WRAPPED coordinator whose own wakers keep it busy)."
  [states rows {:keys [threshold exclude auto-compact?]}]
  (filter (fn [row]
            (let [st (get states (:pane row))]
              (and (:session-id row)
                   (not (some (set exclude) [(:pane row) (:target row) (:name row) (:session-id row)]))
                   (not (and (:ready-fired? st) (= (:session-id st) (:session-id row))))
                   (or (ready/candidate? row threshold)
                       (and auto-compact? (compact/due? row threshold) (not (:silenced? st)))))))
          rows))

;;; Boundary

;;; Auto-arm: the options survive a process restart

(def ^:private persisted-keys
  "The :opts keys that describe WHAT to watch. Ports and seams never persist:
   they are resolved fresh, and a stored fn is not readable anyway."
  [:parent :threshold :interval-ms :exclude :auto-compact?
   :idle-ms :recheck-ms :idle-capacity? :evidence-interval-ms])

(defn options-path
  "Where the watcher's options are persisted:
   $XDG_STATE_HOME/hive-tmux/watch.edn, else ~/.local/state/hive-tmux/watch.edn."
  ([] (options-path {}))
  ([{:keys [xdg-state home] :or {home (System/getProperty "user.home")}}]
   (str (if (str/blank? xdg-state) (str home "/.local/state") xdg-state)
        "/hive-tmux/watch.edn")))

(defn arm-options
  "The opts worth persisting, as data. Pure."
  [opts]
  (select-keys opts persisted-keys))

(defn persist!
  "Write OPTS to PATH so the next process can re-arm. Soft-fails: a watcher
   that cannot record itself still watches. Returns the path, or nil."
  ([opts] (persist! opts (options-path)))
  ([opts path]
   (guard Throwable nil
     (do (io/make-parents (io/file path))
         (spit path (pr-str (arm-options opts)))
         path))))

(defn read-options
  "The persisted opts at PATH, or nil when absent or unreadable."
  [path]
  (when-not (str/blank? (str path))
    (guard Throwable nil
      (let [f (io/file path)]
        (when (.exists f)
          (let [m (read-string (slurp f))]
            (when (map? m) m)))))))

(def shout-sym 'hive-mcp.hivemind.messaging/shout!)

(defonce ^:private watcher (atom nil))

(defn- shout! [{:keys [parent shout-fn]} event data]
  (when-let [f (or shout-fn (guard Throwable nil (requiring-resolve shout-sym)))]
    (guard Throwable nil
      (f (str "tmux:" (or (:tmux-name data) "watch")) event
         (cond-> (assoc data :directory (:tmux-cwd data))
           parent (assoc :parent-id parent))))))

(declare job-ended!)

(defn- current
  "The running watcher's state atom, or nil (tests drive `tick!` with it)."
  []
  @watcher)

(defn- pane-rows
  "The pane rows now: OPTS :panes-fn (host -> rows; the pane-state port's
   seam, for tests), else `panes/panes` over HOST."
  [opts host]
  ((or (:panes-fn opts) panes/panes) host))

(defn- read-evidence
  "Evidence for each of ROWS, by pane id, through OPTS :evidence-fn (row ->
   evidence), each read at most every :evidence-interval-ms per session."
  [w rows now]
  (let [{:keys [evidence-fn evidence-interval-ms]} (:opts @w)]
    (into {}
          (keep (fn [row]
                  (let [k [(:pane row) (:session-id row)]
                        cached (get-in @w [:evidence-cache k])]
                    (if (and cached (< (- now (:at cached)) evidence-interval-ms))
                      [(:pane row) (:evidence cached)]
                      (when evidence-fn
                        (let [e (guard Throwable nil (evidence-fn row))]
                          (swap! w assoc-in [:evidence-cache k] {:at now :evidence e})
                          [(:pane row) e]))))))
          rows)))

(defn- silence!
  "Silence the wakers of ROW's WRAPPED coordinator (decision 7b40b779: its
   own monitor keeps its pane busy, so it can never be compacted) once per
   spell; records it on the pane's state."
  [w row]
  (let [silencer (:silencer (:opts @w))
        r (when silencer (guard Throwable {:error :silence-threw} (silencer row)))]
    (swap! w update-in [:states (:pane row)] assoc :silenced? true)
    (swap! w update :silenced conj {:pane (:pane row) :session-id (:session-id row)
                                    :result (if silencer r :no-silencer)})))

(defn- auto-compact!
  "Compact the compact-ready ROW, as the one compactor: the lease is still
   ours, the pane still shows the SAME session (pane ids are reused), it is
   not the owner's own pane, and the re-seed names the hand-off. Returns the
   job, or {:refused reason}."
  [w row evidence]
  (let [{:keys [host opts]} @w
        {:keys [parent lease threshold start-job!]} opts
        now ((:now-ms host))
        lv (ready/lease-verdict ((:read lease)) parent now false)
        fresh (some #(when (= (:pane %) (:pane row)) %) (pane-rows opts host))
        ;; The compactor's own pane, by the same fail-closed reading the verbs
        ;; use (`caller/identity-of`, card 20261122175413-3d66aa77).
        ident (caller/identity-of parent)]
    (cond
      (not (:ok? lv)) {:refused :lease-held :holder (:holder lv)}
      (:unresolvable ident) {:refused :unresolvable-caller :caller parent}
      (or (ready/own-row? row parent)
          (caller/own-pane? row (:token ident))
          (caller/own-pane? fresh (:token ident))) {:refused :own-pane}
      (not= (:session-id fresh) (:session-id row)) {:refused :session-changed :now (:session-id fresh)}
      :else ((or start-job! compact/start!)
             (:pane fresh)
             {:host host :kind :compact :threshold threshold
              :reseed (ready/reseed-text evidence) :on-end job-ended!}))))

(defn tick!
  "One pass: read the panes and the coordinator evidence, step, shout every
   event, and (auto-compact) silence and compact. The idle-capacity sense
   (`idle-capacity/step`) rides the same pane read, keyed by session id,
   through the watcher's :idle-ports. Returns the events."
  [w]
  (let [{:keys [host opts states ic-states]} @w
        now ((or (:now-ms host) #(System/currentTimeMillis)))
        rows (pane-rows opts host)
        evidence (read-evidence w (evidence-wanted states rows opts) now)
        {next-states :states events :events} (step states rows (assoc opts :now now :evidence evidence))
        ports (:idle-ports opts)
        ic-now (if-let [f (:now-ms ports)] (f) now)
        {ic-next :states ic-events :events}
        (if ports
          (idle-capacity/step ports ic-states rows ic-now (select-keys opts [:idle-ms :recheck-ms :exclude]))
          {:states ic-states :events []})]
    (swap! w assoc :states next-states :ic-states ic-next :last-tick (System/currentTimeMillis)
           :ticks (inc (:ticks @w 0)))
    (when (:auto-compact? opts)
      ((:write! (:lease opts)) {:owner (:parent opts) :renewed-ms now})
      (doseq [row rows
              :let [st (get next-states (:pane row))]
              :when (and st (:crossed-at st) (not (:silenced? st))
                         (ready/evidence-after (get evidence (:pane row)) (:crossed-at st)))]
        (silence! w row)))
    (doseq [[event row extra] events]
      (shout! opts event (assoc (if (= :compact-ready event) (ready-data row extra) (row-data row))
                                :message (message event row)))
      (swap! w update-in [:fired event] (fnil inc 0))
      (when (and (= :compact-ready event) (:auto-compact? opts))
        (let [r (auto-compact! w row extra)]
          (swap! w update :auto-compacts conj
                 (cond-> {:pane (:pane row) :session-id (:session-id row)}
                   (:job-id r) (assoc :job-id (:job-id r))
                   (:refused r) (assoc :refused (:refused r))
                   (:error r) (assoc :error (:error r)))))))
    (doseq [[event row] ic-events]
      (shout! opts event (merge (row-data row) (idle-capacity/event-data row)
                                {:message (idle-capacity/message row)}))
      (swap! w update-in [:fired event] (fnil inc 0)))
    (into (vec events) ic-events)))

(defn running? [] (boolean (some-> @watcher deref :running?)))

(def ^:private port-keys [:host :shout-fn :evidence-fn :silencer :lease :start-job! :panes-fn :idle-ports])

(defn stop!
  "Stop the watcher, releasing the compactor lease it holds. Returns its last
   status, or nil when none ran."
  []
  (when-let [w @watcher]
    (swap! w assoc :running? false)
    (reset! watcher nil)
    ;; A deliberate stop disarms persistence too, so the next boot does not
    ;; resurrect a watcher the operator turned off.
    (guard Throwable nil (io/delete-file (io/file (options-path)) true))
    (let [{:keys [opts]} @w]
      (when (:auto-compact? opts)
        (let [{:keys [read write!]} (:lease opts)]
          (when (= (:parent opts) (:owner (guard Exception nil (read))))
            (guard Exception nil (write! nil))))))
    (-> @w (dissoc :host :states :ic-states :evidence-cache)
        (update :opts #(apply dissoc % port-keys)))))

(defn start!
  "Start (or restart) the watcher. OPTS: :parent (agent id the shouts are
   addressed to; required), :threshold, :interval-ms, :exclude (refs),
   :auto-compact? (compact every compact-ready pane, as the ONE compactor:
   takes the host-wide lease, refused while another owner holds it fresh
   unless :takeover?), :idle-ms / :recheck-ms (idle-capacity), :idle-ports
   (the idle-capacity ports; default `idle-capacity/default-ports`),
   :idle-capacity? false (turn that sense off), and the ports :host :shout-fn
   :evidence-fn :lease :silencer :start-job! (tests; the defaults read the host)."
  [{:keys [parent auto-compact? takeover?] :as opts}]
  (when (str/blank? (str parent))
    (throw (ex-info "watch needs parent: the agent id that should hear the senses" {:type :tmux/no-parent})))
  (let [host (or (:host opts) (host/host))
        lease (or (:lease opts) (ready/file-lease))
        now ((:now-ms host))
        lv (when auto-compact? (ready/lease-verdict ((:read lease)) parent now takeover?))]
    (if (and lv (not (:ok? lv)))
      {:running? (running?) :refused :lease-held :holder (:holder lv) :renewed-ms (:renewed-ms lv)
       :hint (str "Another compactor (" (:holder lv) ") holds the lease (decision 20261121215259-7b40b779: exactly one). "
                  "Start without auto_compact, or pass takeover=true if that owner is gone.")}
      (do
        (stop!)
        (let [opts (cond-> (merge defaults opts {:host host :lease lease})
                     (not (contains? opts :evidence-fn)) (assoc :evidence-fn (ready/host-evidence))
                     (not (contains? opts :silencer)) (assoc :silencer (ready/host-silencer))
                     (and (not (contains? opts :idle-ports)) (not (false? (:idle-capacity? opts))))
                     (assoc :idle-ports (idle-capacity/default-ports)))
              w (atom {:running? true :opts opts :host host
                       :states {} :started-at (System/currentTimeMillis) :ticks 0 :fired {}
                       :silenced [] :auto-compacts []})]
          (when auto-compact? ((:write! lease) {:owner parent :renewed-ms now}))
          ;; Persist WHAT to watch BEFORE the thread starts, so a process that
          ;; dies mid-watch still leaves the next boot something to re-arm from.
          (persist! opts (:options-path opts))
          (reset! watcher w)
          (future
            (while (:running? @w)
              (try (tick! w)
                   (catch Throwable t
                     (swap! w assoc :last-error (.getMessage t))
                     (log/warn "[claude-watch] tick failed" (.getMessage t))))
              (Thread/sleep (long (:interval-ms opts)))))
          (-> @w (dissoc :host :states :ic-states) (update :opts #(apply dissoc % port-keys))))))))

(defn status []
  (if-let [w @watcher]
    (-> @w (dissoc :host :ic-states :evidence-cache) (update :states count)
        (update :opts #(apply dissoc % port-keys)) (assoc :running? true))
    {:running? false}))

(defn arm!
  "Re-arm the watcher at PATH when it is not already running: the answer a
   fresh process gives on boot, so a restart does not silently disarm the
   supervision the operator left running. A missing or unreadable file is a
   no-op (:reason :no-options: nobody ever armed one)."
  ([] (arm! (options-path)))
  ([path]
   (if (running?)
     {:armed? false :reason :already-running}
     (if-let [opts (read-options path)]
       (let [r (start! (merge opts {:host (host/host)}))]
         (if (:running? r)
           {:armed? true :path path :parent (:parent opts)}
           {:armed? false :reason (:refused r)}))
       {:armed? false :reason :no-options :path path}))))

(defn job-ended!
  "Shout a finished compact | say | goal JOB to the running watcher's parent.
   A no-op when no watcher runs: nobody asked to hear it."
  [job]
  (when-let [w @watcher]
    (let [opts (:opts @w)]
      (shout! opts :tmux-job
              (into {} (remove (comp nil? val))
                    {:message (str (name (:kind job :compact)) " " (:job-id job) " on " (or (:name job) (:pane job) (:ref job)) ": "
                                   (name (:status job)) (some->> (:reason job) name (str " ")))
                     :tmux-pane (:pane job) :tmux-name (:name job)
                     :tmux-job-id (:job-id job) :tmux-job-kind (some-> (:kind job) name)
                     :tmux-job-status (some-> (:status job) name)
                     :tmux-job-reason (some-> (:reason job) name)})))))

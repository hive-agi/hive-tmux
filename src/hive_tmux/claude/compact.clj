(ns hive-tmux.claude.compact
  "Compact a Claude Code pane and bring it back to work: wait until it is
   idle, remember its /goal, send /compact, re-seed it with a prompt, then
   set the goal again.

   The goal step exists because /compact clears an active /goal: a peer that
   is re-seeded with catchup alone finishes catchup, finds no task and stops.

   WAITING FOR REST. A job must not type into a working turn, so every step
   waits for rest first. A pane busy for ordinary reasons is read at a settled
   idle (idle twice, :settle-ms apart); a pane under a /goal Stop hook is read
   at its single turn boundary, because the hook restarts the next turn within
   a poll and twice-in-a-row can never happen on it (friction 20261116185504:
   30 min at :wait-idle, then 337k -> 469k tokens). `poll/wait-rest` is the one
   waiter, so the choice holds across every step of a job.

   Strata:
     Promote   due? / ctx-drop / goal-after          pure
     Pipeline  compact! / say! / goal!               drive the steps through the host port
               (waiting: `poll`; typing: `composer`)
     Boundary  start! / job / jobs                   one background job per pane

   Every effect (tmux, transcript, clock, sleep) goes through the host port,
   so the whole sequence runs in a test against a scripted screen."
  (:require [clojure.string :as str]
            [hive-dsl.result :refer [guard]]
            [hive-tmux.claude.composer :as composer]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.panes :as panes]
            [hive-tmux.claude.poll :as poll]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def default-reseed "hive workflow catchup /caveman")

(def defaults
  {:threshold 400000
   :reseed default-reseed
   :restore-goal? true
   :poll-ms 5000
   :settle-ms 10000
   ;; How rest is proven: :auto reads a /goal-active pane at its single turn
   ;; boundary and every other pane at a settled idle; true and false force
   ;; one or the other (`between-turns?`).
   :between-turns? :auto
   :idle-timeout-ms (* 30 60 1000)
   :step-timeout-ms (* 15 60 1000)
   :start-timeout-ms 60000
   ;; Text someone left at the prompt is waited out this long (until it is
   ;; sent or cleared and the pane is idle again) before a send gives up as
   ;; :pending-human-input. Nothing is ever typed over it.
   :input-timeout-ms (* 10 60 1000)
   ;; /compact is proven when the context falls under this share of what it
   ;; was, or by a new compact_boundary line in the transcript.
   :drop-ratio 0.8})

;;; Promote

(def idle?
  "Is ROW a Claude pane at rest? See `poll/idle?`."
  poll/idle?)

(defn due?
  "Should ROW be compacted at THRESHOLD tokens?"
  [row threshold]
  (boolean (some-> (:ctx-tokens row) (>= threshold))))

(defn screen-state
  "What a timeout reports of the pane's last ROW: enough for a supervisor to
   tell a slow pane from a stuck one without reading the screen itself."
  [row]
  (when row
    (into {} (remove (comp nil? val))
          (select-keys row [:ctx-tokens :busy? :activity :status :goal-active? :input :suggestion]))))

(defn ctx-drop
  "Evidence from ROW that a compaction ran: its context fell below DROP-RATIO
   of BEFORE tokens. {:by :ctx-drop :tokens n}, or nil."
  [before row drop-ratio]
  (let [now (:ctx-tokens row)]
    (when (and (number? before) (pos? before) (number? now) (< now (* drop-ratio before)))
      {:by :ctx-drop :tokens now})))

(defn boundary-seen
  "Evidence from BOUNDARIES (`panes/compact-boundaries` of the transcript now)
   that a compaction finished after the BEFORE-COUNT boundaries there were
   when /compact was sent. {:by :compact-boundary :tokens post}, or nil."
  [before-count boundaries]
  (when (and (some? before-count) (> (count boundaries) before-count))
    {:by :compact-boundary :tokens (:post-tokens (peek boundaries))}))

(defn cancelled?
  "Has JOB been cancelled? Terminal: nothing the job's runner writes after
   it may change its status (`new-state` enforces it)."
  [job]
  (= :cancelled (:status job)))

(defn cancel-verdict
  "May JOB (a job value) be cancelled now? {:cancel? true}, or a refusal:
   :already-typed with the :step that typed first (a keystroke reached the
   pane, so cancelling would leave half a command there), :already-cancelled,
   or :not-active with the job's :status (it ended on its own)."
  [job]
  (cond (cancelled? job) {:refuse :already-cancelled}
        (:typed-step job) {:refuse :already-typed :step (:typed-step job)}
        (not (#{:queued :running} (:status job))) {:refuse :not-active :status (:status job)}
        :else {:cancel? true}))

(defn cancel-job
  "JOB cancelled at NOW-MS for REASON, when `cancel-verdict` allows it; JOB
   unchanged otherwise."
  [job reason now-ms]
  (if (:cancel? (cancel-verdict job))
    (assoc job :status :cancelled :reason reason :cancelled-at now-ms :ended-at now-ms)
    job))

(defn claim-typing
  "JOB about to type its STEP-KEY step: records the first step that types
   (:typed-step), after which `cancel-verdict` refuses. A cancelled JOB is
   returned unchanged, and must not type."
  [job step-key]
  (cond (cancelled? job) job
        (:typed-step job) job
        :else (assoc job :typed-step step-key)))

(defn job-consistent?
  "The job invariant a job's state atom validates: once cancelled (it carries
   :cancelled-at), a job stays :cancelled. A runner still waiting when its job
   was cancelled therefore cannot write a later status over it: the write is
   refused and the runner stops there."
  [job]
  (or (not (contains? job :cancelled-at)) (cancelled? job)))

;;; Pipeline

(defn- step! [state k data]
  (swap! state update :steps conj (assoc data :step k))
  (log/debug "[claude-compact]" k data))

(defn- fail! [state reason data]
  (swap! state assoc :status :failed :reason reason :error data)
  nil)

(defn- timeout!
  "Fail as :timeout DURING STEP-KEY, naming what the step waited for (DATA)
   and the pane's last screen state, so a supervisor can tell a slow pane from
   a stuck one and from a step that in fact succeeded."
  [h state ref step-key data]
  (fail! state :timeout (merge {:during step-key :last (screen-state (poll/observe h ref))} data)))

(defn- deliver!
  "Type TEXT once the prompt is free (no one's text there, pane idle); true
   when it was submitted. Text someone left at the prompt is never typed over:
   it is waited out (`poll/wait-input-clear`, which also outlasts the turn it
   starts once sent) for at most :input-timeout-ms, while the job shows it
   under :waiting-on, and not at all when that is 0 or :wait-idle? is false.
   Still there, the job stops :skipped :pending-human-input naming the pane
   and the text; a pane that stayed busy stops it :skipped :busy. A slash
   command the composer does not hold back stops it :failed
   :composer-mismatch with Enter unpressed (`composer/send-text!`).
   The first keystroke is claimed on STATE (`claim-typing`) in the same swap
   a cancel would make, so a job cancelled before it types never types."
  [h state ref pane-id text step-key opts]
  (let [row0 (poll/observe h ref)
        free? (and (poll/idle? row0) (nil? (:input row0)))
        wait-ms (if (false? (:wait-idle? opts)) 0 (or (:input-timeout-ms opts) 0))
        on (if (:input row0) {:pending-human-input (:input row0)} {:pane-busy true})
        row (cond free? row0
                  (pos? wait-ms) (do (step! state :wait-input (assoc on :during step-key :timeout-ms wait-ms))
                                     (swap! state assoc :waiting-on (assoc on :during step-key))
                                     (try (poll/wait-input-clear h ref opts wait-ms)
                                          (finally (swap! state dissoc :waiting-on)))))]
    (cond
      (cancelled? @state) false

      (not row)
      (let [end (or (poll/observe h ref) row0)
            input (or (:input end) (:input row0))]
        (swap! state assoc :status :skipped :reason (if input :pending-human-input :busy)
               :error (cond-> {:during step-key :pane pane-id :name (:name end)
                               :waited-ms wait-ms :last (screen-state end)}
                        input (assoc :input input
                                     :hint (str "Text stands at the prompt of " pane-id " and was not typed over. "
                                                "Send or clear it, then start the job again."))))
        false)

      (cancelled? (swap! state claim-typing step-key)) false

      :else
      (let [{:keys [submitted? composer]} (composer/send-text! h pane-id text (:vim-mode row))]
        (if submitted?
          (do (step! state step-key {:sent text}) true)
          (fail! state :composer-mismatch
                 {:during step-key :pane pane-id :expected text :composer composer
                  :hint "Enter was not pressed: the composer does not hold the command as typed. Clear it, then start the job again."}))))))

(defn- send-and-settle!
  "Send TEXT, wait for the turn it starts to end. Returns the idle row, or nil
   when it stopped (status is set). For an intermediate step: the next send
   must not land on a working pane."
  [h state ref pane-id text step-key opts]
  (when (deliver! h state ref pane-id text step-key opts)
    ;; The turn may start and end between two polls; seeing busy is not required.
    (poll/wait-until h ref :busy? opts (:start-timeout-ms opts))
    (or (poll/wait-rest h ref poll/idle? opts (:step-timeout-ms opts))
        (timeout! h state ref step-key {:waited-for (poll/waited-for opts)}))))

(defn- send-and-start!
  "Send TEXT as a job's LAST step and end on the pane's acknowledgement: seen
   working, or, for a /goal, the goal shown active, within :start-timeout-ms.
   Waiting for idle here would wait on the very work the send restarts.
   {:started? bool :row row}; an unacknowledged delivery is :started? false,
   not a hang. Nil when the delivery stopped it (status is set)."
  [h state ref pane-id text step-key opts]
  (when (deliver! h state ref pane-id text step-key opts)
    (let [goal? (str/starts-with? text "/goal ")
          seen (poll/wait-until h ref #(boolean (or (:busy? %) (and goal? (:goal-active? %))))
                                opts (:start-timeout-ms opts))]
      {:started? (boolean seen) :row (or seen (poll/observe h ref))})))

(defn- boundaries-of
  "The compact boundaries in the transcript of ROW's session, or nil when it
   cannot be read."
  [h row]
  (when (:session-id row)
    (guard Exception nil
      (panes/compact-boundaries (or ((:transcript h) (:cwd row) (:session-id row)) [])))))

(defn- send-and-compact!
  "Send the /compact TEXT and wait for proof that the compaction ran, not for
   a screen marker: the context count fell under :drop-ratio of BEFORE, or a
   new compact_boundary line is in the transcript. Neither within
   :step-timeout-ms is a :timeout naming the step and the last screen. Proof
   seen, wait for a settled idle so the re-seed does not land on a working
   pane. Returns {:compacted evidence :row idle-row-or-nil}, or nil when it
   stopped (status is set)."
  [h state ref pane-id text before opts]
  (let [{:keys [now-ms sleep!]} h
        n0 (some-> (boundaries-of h (poll/observe h ref)) count)]
    (when (deliver! h state ref pane-id text :compact opts)
      (let [deadline (+ (now-ms) (:step-timeout-ms opts))
            evidence (loop []
                       (let [row (poll/observe h ref)
                             ev (or (ctx-drop before row (:drop-ratio opts))
                                    (boundary-seen n0 (boundaries-of h row)))]
                         (cond ev ev
                               (>= (now-ms) deadline) nil
                               :else (do (sleep! (:poll-ms opts)) (recur)))))]
        (if-not evidence
          (timeout! h state ref :compact {:waited-for :compaction :tokens-before before})
          (do (step! state :compacted evidence)
              (swap! state assoc :compacted? true :compacted-by (:by evidence)
                     :tokens-after-compact (:tokens evidence))
              {:compacted evidence
               :row (poll/wait-rest h ref poll/idle? opts (:step-timeout-ms opts))}))))))

(defn goal-after
  "The goal to set after the re-seed. GOAL (an override) wins over CAPTURED;
   APPEND is added to whichever applies. Nil when there is nothing to set."
  [{:keys [goal goal-append]} captured]
  (let [base (or (some-> goal str/trim not-empty) captured)
        extra (some-> goal-append str/trim not-empty)]
    (cond (and base extra) (str base " " extra)
          :else (or base extra))))

(defn compact!
  "Compact the pane REF names, through host H, recording progress in STATE
   (an atom). OPTS: see `defaults`, plus :force? (ignore the threshold),
   :instructions (text after /compact), :goal (replace the goal) and
   :goal-append (extend it). Returns STATE's final value.

   The /compact step ends on proof that the compaction ran (`send-and-compact!`),
   never on a screen marker. A compaction proven while the pane then stays busy
   (someone else gave it work) is :done with :reason :pane-busy-after-compact
   and the texts it did not type under :unsent: the recycle worked, and typing
   into a working pane is what this job never does.

   REST: see the namespace docstring. Nothing here lands on a running turn."
  [h state ref opts]
  (let [opts (merge defaults opts)
        start (poll/observe h ref)
        ;; Proving rest is decided ONCE, from the pane as the job found it: the
        ;; /goal that makes a Stop hook restart the next turn is what /compact
        ;; clears, so asking later would flip the job's contract halfway.
        opts (poll/with-turns opts start)]
    (swap! state assoc :status :running :pane (:pane start) :name (:name start)
           :tokens-before (:ctx-tokens start) :between-turns? (:between-turns? opts))
    (cond
      (nil? start)
      (fail! state :no-such-pane {:ref ref})

      (not (or (:force? opts) (due? start (:threshold opts))))
      (swap! state assoc :status :skipped :reason :under-threshold)

      :else
      (let [pane-id (:pane start)]
        (when-let [ready (do (step! state :wait-idle {})
                             (or (poll/wait-rest h ref poll/idle? opts (:idle-timeout-ms opts))
                                 (timeout! h state ref :wait-idle {:waited-for (poll/waited-for opts)})))]
          (let [captured (when (and (:restore-goal? opts) (:goal-active? ready) (:session-id ready))
                           (panes/goal-text (or ((:transcript h) (:cwd ready) (:session-id ready)) [])))
                goal (goal-after opts captured)
                reseed (when-not (str/blank? (:reseed opts)) (:reseed opts))
                sends (cond-> [] reseed (conj [reseed :reseed]) goal (conj [(str "/goal " goal) :goal-restored]))]
            (step! state :goal-captured {:captured captured :goal goal
                                         :goal-active? (:goal-active? ready)})
            (swap! state assoc :goal goal)
            (when (and (:goal-active? ready) (:restore-goal? opts) (nil? captured) (nil? (:goal opts)))
              (step! state :goal-unreadable {:note "goal active but no /goal text in the transcript; it will not be restored"}))
            (when-let [{:keys [row]} (send-and-compact! h state ref pane-id
                                                        (str "/compact" (some->> (:instructions opts) str/trim not-empty (str " ")))
                                                        (:ctx-tokens ready) opts)]
              (cond
                (and (nil? row) (seq sends))
                (swap! state assoc :status :done :reason :pane-busy-after-compact
                       :unsent (mapv first sends) :last (screen-state (poll/observe h ref)))

                ;; The last send restarts work: it ends the job once acknowledged.
                :else
                (let [last-send (peek sends)]
                  (when (or (< (count sends) 2)
                            (send-and-settle! h state ref pane-id reseed :reseed opts))
                    (if-let [[text k] last-send]
                      (when-let [{:keys [started? row]} (send-and-start! h state ref pane-id text k opts)]
                        (swap! state assoc :status :done :started? started?
                               :tokens-after (:ctx-tokens row) :goal-active-after? (:goal-active? row)))
                      (let [end (poll/observe h ref)]
                        (swap! state assoc :status :done :tokens-after (:ctx-tokens end)
                               :goal-active-after? (:goal-active? end))))))))))))
    @state))

(defn- start-row
  "The pane's first row, waited to a settled idle when WAIT-IDLE? (else it
   must already be idle). Nil when it stopped (status is set)."
  [h state ref {:keys [wait-idle?] :as opts}]
  (let [start (poll/observe h ref)
        opts (poll/with-turns opts start)]
    (swap! state assoc :status :running :pane (:pane start) :name (:name start)
           :tokens-before (:ctx-tokens start) :between-turns? (:between-turns? opts))
    (cond
      (nil? start) (fail! state :no-such-pane {:ref ref})
      wait-idle? (do (step! state :wait-idle {})
                     (or (poll/wait-rest h ref poll/idle? opts (:idle-timeout-ms opts))
                         (timeout! h state ref :wait-idle {:waited-for (poll/waited-for opts)})))
      (idle? start) start
      :else (do (swap! state assoc :status :skipped :reason :busy) nil))))

(defn say!
  "Submit OPTS :text to the pane REF names as one prompt. Waits for a settled
   idle first unless :wait-idle? is false (then a busy pane is skipped). Done
   once the pane starts working on it, or settles: the reply itself can take
   an hour, read it later with `read`. Text at the prompt stops the job at
   once: a say is not queued behind someone's draft."
  [h state ref opts]
  (let [opts (merge defaults {:wait-idle? true} opts)
        text (some-> (:text opts) str/trim not-empty)]
    (if-not text
      (fail! state :no-text {})
      (when-let [ready (start-row h state ref opts)]
        (when (deliver! h state ref (:pane ready) text :say (assoc opts :input-timeout-ms 0))
          (let [started (poll/wait-until h ref :busy? opts (:start-timeout-ms opts))]
            (swap! state assoc :status :done :delivered true
                   :started? (boolean started)))))))
  @state)

(defn goal!
  "Set the /goal of the pane REF names without compacting it: OPTS :goal
   replaces, :goal-append extends the pane's active goal (read from its
   transcript). Waits for a settled idle; text at the prompt is waited out up
   to :input-timeout-ms (`deliver!`), then stops the job."
  [h state ref opts]
  (let [opts (merge defaults {:wait-idle? true} opts)]
    (when-let [ready (start-row h state ref opts)]
      (let [captured (when (and (:goal-active? ready) (:session-id ready))
                       (panes/goal-text (or ((:transcript h) (:cwd ready) (:session-id ready)) [])))
            goal (goal-after opts captured)]
        (swap! state assoc :goal goal :captured captured)
        (if-not goal
          (fail! state :no-goal {:note "pass goal, or goal_append on a pane whose goal is active"})
          (when-let [{:keys [started? row]} (send-and-start! h state ref (:pane ready) (str "/goal " goal) :goal-set opts)]
            (swap! state assoc :status :done :started? started?
                   :goal-active-after? (:goal-active? row)))))))
  @state)

;;; Boundary

(defonce ^:private registry (atom {}))

(defn- new-job [ref opts]
  {:job-id (str "cc-" (subs (str (random-uuid)) 0 8))
   :ref ref :status :queued :steps [] :opts (dissoc opts :host)
   :started-at (System/currentTimeMillis)})

(defn- active-for [pane-id]
  (some (fn [[_ s]] (let [v @s]
                      (when (and (= pane-id (:pane v)) (#{:queued :running} (:status v)))
                        v)))
        @registry))

(defn cancel!
  "Cancel JOB-ID for REASON (:cancelled by default) if it has not typed yet
   (`cancel-verdict`, decided inside the swap, so it cannot race the job's
   first keystroke). The job ends :cancelled, which frees its pane's slot,
   and its :on-end runs once with the cancelled job (the tmux-job sense).
   {:cancelled? true :job job}, or {:cancelled? false :refuse kw ...} naming
   why, e.g. :already-typed with the :step; nil when there is no such job."
  ([job-id] (cancel! job-id :cancelled))
  ([job-id reason]
   (when-let [state (get @registry job-id)]
     (let [[before after] (swap-vals! state cancel-job reason (System/currentTimeMillis))]
       (if (and (not (cancelled? before)) (cancelled? after))
         (let [on-end (:on-end (meta state))]
           (when on-end (guard Exception nil (on-end after)))
           {:cancelled? true :job after})
         (merge {:cancelled? false :job (dissoc before :steps)} (cancel-verdict before)))))))

(def ^:private runners
  {:compact #'compact!
   :say #'say!
   :goal #'goal!})

(defn- cancellable-host
  "H whose :sleep! ends the runner of STATE once its job is cancelled, so a
   cancelled job stops at its next wait instead of polling the pane for up to
   :idle-timeout-ms. Every wait sleeps through the host port, so this is the
   one seam a runner needs."
  [{:keys [sleep!] :as h} state]
  (assoc h :sleep! (fn [ms]
                     (when (cancelled? @state)
                       (throw (ex-info "job cancelled" {:type ::cancelled :job-id (:job-id @state)})))
                     (sleep! ms))))

(defn start!
  "Start a background job on the pane REF names. OPTS :kind is :compact
   (default), :say or :goal. Returns the job map at once, or {:error ...} when
   that pane already has a running job. The job follows the pane by its tmux
   id, which /compact does not change. OPTS :on-end, a fn of the final job
   map, runs once when the job ends (how the watcher turns it into a sense).
   Pass :host in OPTS to use another port (tests); :sync? true runs in the
   caller.

   A job cancelled (`cancel!`) before its first keystroke ends :cancelled:
   its state refuses any later status (`job-consistent?`), its runner stops at
   its next wait (`cancellable-host`), and :on-end runs once, from `cancel!`."
  [ref {:keys [sync? on-end] :as opts}]
  (let [h (or (:host opts) (host/host))
        kind (or (:kind opts) :compact)
        runner (get runners kind)
        row (poll/observe h ref)]
    (cond
      (nil? runner) {:error :unknown-kind :kind kind}
      (and row (active-for (:pane row))) {:error :already-running :job (active-for (:pane row))}
      :else
      (let [job (assoc (new-job ref (dissoc opts :on-end)) :kind kind)
            state (atom (assoc job :pane (:pane row))
                        :meta {:on-end on-end}
                        :validator job-consistent?)
            follow (or (:pane row) ref)
            h (cancellable-host h state)]
        (swap! registry assoc (:job-id job) state)
        (let [work #(try (runner h state follow (dissoc opts :host :sync? :kind :on-end))
                         (catch Throwable t
                           (when-not (cancelled? @state)
                             (fail! state :threw {:message (.getMessage t)}))
                           @state)
                         (finally
                           ;; A cancelled job was ended, and announced, by `cancel!`.
                           (when-not (cancelled? @state)
                             (swap! state assoc :ended-at (System/currentTimeMillis))
                             (when on-end (guard Exception nil (on-end @state))))))]
          (if sync?
            (work)
            (do (future (work)) @state)))))))

(defn job
  "The current state of JOB-ID, or nil."
  [job-id]
  (some-> (get @registry job-id) deref))

(defn jobs
  "Every job, newest first."
  []
  (->> (vals @registry) (map deref) (sort-by :started-at >) vec))

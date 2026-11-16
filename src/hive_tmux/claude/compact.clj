(ns hive-tmux.claude.compact
  "Compact a Claude Code pane and bring it back to work: wait until it is
   idle, remember its /goal, send /compact, re-seed it with a prompt, then
   set the goal again.

   The goal step exists because /compact clears an active /goal: a peer that
   is re-seeded with catchup alone finishes catchup, finds no task and stops.

   Strata:
     Promote   idle? / plan-steps          pure
     Pipeline  run!                        drives the steps through the host port
     Boundary  start! / job / jobs         one background job per pane

   Every effect (tmux, transcript, clock, sleep) goes through the host port,
   so the whole sequence runs in a test against a scripted screen."
  (:require [clojure.string :as str]
            [hive-dsl.result :refer [guard]]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.panes :as panes]
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
   :idle-timeout-ms (* 30 60 1000)
   :step-timeout-ms (* 15 60 1000)
   :start-timeout-ms 60000
   ;; /compact is proven when the context falls under this share of what it
   ;; was, or by a new compact_boundary line in the transcript.
   :drop-ratio 0.8})

;;; Promote

(defn idle?
  "Is ROW a Claude pane at rest? The screen shows no spinner and Claude Code's
   own session status, when known, is not busy. A row that could not be read
   is not idle."
  [row]
  (boolean (and row (some? (:ctx-tokens row)) (not (:busy? row)))))

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

;;; Pipeline

(defn- observe [h ref]
  (guard Exception nil (panes/pane h ref)))

(defn- wait-until
  "Poll (observe) until (pred row) holds, at most TIMEOUT-MS. The row, or nil
   on timeout."
  [h ref pred {:keys [poll-ms]} timeout-ms]
  (let [{:keys [now-ms sleep!]} h
        deadline (+ (now-ms) timeout-ms)]
    (loop []
      (let [row (observe h ref)]
        (cond (pred row) row
              (>= (now-ms) deadline) nil
              :else (do (sleep! poll-ms) (recur)))))))

(defn- wait-settled-idle
  "Idle twice, SETTLE-MS apart: a pane between two tool calls looks idle for
   a moment. The second row, or nil on timeout."
  [h ref {:keys [settle-ms] :as opts} timeout-ms]
  (let [{:keys [now-ms sleep!]} h
        deadline (+ (now-ms) timeout-ms)]
    (loop []
      (when-let [first-idle (wait-until h ref idle? opts (max 0 (- deadline (now-ms))))]
        (sleep! settle-ms)
        (let [row (observe h ref)]
          (cond (idle? row) (or row first-idle)
                (>= (now-ms) deadline) nil
                :else (recur)))))))

(defn send-text!
  "Type TEXT into PANE-ID and submit it. One line goes as literal keys; text
   with newlines is pasted as one bracketed frame so no LF submits early. A
   pane in vim NORMAL mode is put in insert mode first."
  [{:keys [tmux! sleep!]} pane-id text vim-mode]
  (when (= :normal vim-mode)
    (tmux! ["send-keys" "-t" pane-id "i"]))
  (if (str/includes? text "\n")
    (let [buf (str "hive-claude-" (random-uuid))]
      (tmux! ["load-buffer" "-b" buf "-"] text)
      (tmux! ["paste-buffer" "-p" "-r" "-d" "-b" buf "-t" pane-id]))
    (tmux! ["send-keys" "-t" pane-id "-l" text]))
  (sleep! 300)
  (tmux! ["send-keys" "-t" pane-id "Enter"])
  nil)

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
  (fail! state :timeout (merge {:during step-key :last (screen-state (observe h ref))} data)))

(defn- deliver!
  "Type TEXT unless text already stands at the prompt: that is someone's
   unsent input, nothing is typed over it and the job stops as
   :pending-human-input. True when TEXT was sent."
  [h state ref pane-id text step-key]
  (let [row (observe h ref)]
    (if-let [input (:input row)]
      (do (swap! state assoc :status :skipped :reason :pending-human-input
                 :error {:during step-key :input input})
          false)
      (do (send-text! h pane-id text (:vim-mode row))
          (step! state step-key {:sent text})
          true))))

(defn- send-and-settle!
  "Send TEXT, wait for the turn it starts to end. Returns the idle row, or nil
   when it stopped (status is set). For an intermediate step: the next send
   must not land on a working pane."
  [h state ref pane-id text step-key opts]
  (when (deliver! h state ref pane-id text step-key)
    ;; The turn may start and end between two polls; seeing busy is not required.
    (wait-until h ref :busy? opts (:start-timeout-ms opts))
    (or (wait-settled-idle h ref opts (:step-timeout-ms opts))
        (timeout! h state ref step-key {:waited-for :settled-idle}))))

(defn- send-and-start!
  "Send TEXT as a job's LAST step and end on the pane's acknowledgement: seen
   working, or, for a /goal, the goal shown active, within :start-timeout-ms.
   Waiting for idle here would wait on the very work the send restarts.
   {:started? bool :row row}; an unacknowledged delivery is :started? false,
   not a hang. Nil when pending input stopped it (status is set)."
  [h state ref pane-id text step-key opts]
  (when (deliver! h state ref pane-id text step-key)
    (let [goal? (str/starts-with? text "/goal ")
          seen (wait-until h ref #(boolean (or (:busy? %) (and goal? (:goal-active? %))))
                           opts (:start-timeout-ms opts))]
      {:started? (boolean seen) :row (or seen (observe h ref))})))

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
        n0 (some-> (boundaries-of h (observe h ref)) count)]
    (when (deliver! h state ref pane-id text :compact)
      (let [deadline (+ (now-ms) (:step-timeout-ms opts))
            evidence (loop []
                       (let [row (observe h ref)
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
               :row (wait-settled-idle h ref opts (:step-timeout-ms opts))}))))))

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
   into a working pane is what this job never does."
  [h state ref opts]
  (let [opts (merge defaults opts)
        start (observe h ref)]
    (swap! state assoc :status :running :pane (:pane start) :name (:name start)
           :tokens-before (:ctx-tokens start))
    (cond
      (nil? start)
      (fail! state :no-such-pane {:ref ref})

      (not (or (:force? opts) (due? start (:threshold opts))))
      (swap! state assoc :status :skipped :reason :under-threshold)

      :else
      (let [pane-id (:pane start)]
        (when-let [ready (do (step! state :wait-idle {})
                             (or (wait-settled-idle h ref opts (:idle-timeout-ms opts))
                                 (timeout! h state ref :wait-idle {:waited-for :settled-idle})))]
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
                       :unsent (mapv first sends) :last (screen-state (observe h ref)))

                ;; The last send restarts work: it ends the job once acknowledged.
                :else
                (let [last-send (peek sends)]
                  (when (or (< (count sends) 2)
                            (send-and-settle! h state ref pane-id reseed :reseed opts))
                    (if-let [[text k] last-send]
                      (when-let [{:keys [started? row]} (send-and-start! h state ref pane-id text k opts)]
                        (swap! state assoc :status :done :started? started?
                               :tokens-after (:ctx-tokens row) :goal-active-after? (:goal-active? row)))
                      (let [end (observe h ref)]
                        (swap! state assoc :status :done :tokens-after (:ctx-tokens end)
                               :goal-active-after? (:goal-active? end))))))))))))
    @state))

(defn- start-row
  "The pane's first row, waited to a settled idle when WAIT-IDLE? (else it
   must already be idle). Nil when it stopped (status is set)."
  [h state ref {:keys [wait-idle?] :as opts}]
  (let [start (observe h ref)]
    (swap! state assoc :status :running :pane (:pane start) :name (:name start)
           :tokens-before (:ctx-tokens start))
    (cond
      (nil? start) (fail! state :no-such-pane {:ref ref})
      wait-idle? (do (step! state :wait-idle {})
                     (or (wait-settled-idle h ref opts (:idle-timeout-ms opts))
                         (timeout! h state ref :wait-idle {:waited-for :settled-idle})))
      (idle? start) start
      :else (do (swap! state assoc :status :skipped :reason :busy) nil))))

(defn say!
  "Submit OPTS :text to the pane REF names as one prompt. Waits for a settled
   idle first unless :wait-idle? is false (then a busy pane is skipped). Done
   once the pane starts working on it, or settles: the reply itself can take
   an hour, read it later with `read`. Text at the prompt stops the job."
  [h state ref opts]
  (let [opts (merge defaults {:wait-idle? true} opts)
        text (some-> (:text opts) str/trim not-empty)]
    (if-not text
      (fail! state :no-text {})
      (when-let [_ready (start-row h state ref opts)]
        (let [row (observe h ref)]
          (if-let [input (:input row)]
            (swap! state assoc :status :skipped :reason :pending-human-input
                   :error {:during :say :input input})
            (do (send-text! h (:pane row) text (:vim-mode row))
                (step! state :say {:sent text})
                (let [started (wait-until h ref :busy? opts (:start-timeout-ms opts))]
                  (swap! state assoc :status :done :delivered true
                         :started? (boolean started)))))))))
  @state)

(defn goal!
  "Set the /goal of the pane REF names without compacting it: OPTS :goal
   replaces, :goal-append extends the pane's active goal (read from its
   transcript). Waits for a settled idle; text at the prompt stops the job."
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

(def ^:private runners
  {:compact #'compact!
   :say #'say!
   :goal #'goal!})

(defn start!
  "Start a background job on the pane REF names. OPTS :kind is :compact
   (default), :say or :goal. Returns the job map at once, or {:error ...} when
   that pane already has a running job. The job follows the pane by its tmux
   id, which /compact does not change. OPTS :on-end, a fn of the final job
   map, runs once when the job ends (how the watcher turns it into a sense).
   Pass :host in OPTS to use another port (tests); :sync? true runs in the
   caller."
  [ref {:keys [sync? on-end] :as opts}]
  (let [h (or (:host opts) (host/host))
        kind (or (:kind opts) :compact)
        runner (get runners kind)
        row (observe h ref)]
    (cond
      (nil? runner) {:error :unknown-kind :kind kind}
      (and row (active-for (:pane row))) {:error :already-running :job (active-for (:pane row))}
      :else
      (let [job (assoc (new-job ref (dissoc opts :on-end)) :kind kind)
            state (atom (assoc job :pane (:pane row)))
            follow (or (:pane row) ref)]
        (swap! registry assoc (:job-id job) state)
        (let [work #(try (runner h state follow (dissoc opts :host :sync? :kind :on-end))
                         (catch Throwable t
                           (fail! state :threw {:message (.getMessage t)})
                           @state)
                         (finally
                           (swap! state assoc :ended-at (System/currentTimeMillis))
                           (when on-end (guard Exception nil (on-end @state)))))]
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

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
   :start-timeout-ms 60000})

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

(defn- send-and-settle!
  "Send TEXT, wait for the turn it starts to end. Returns the idle row, or nil
   when it timed out (status :failed is set)."
  [h state ref pane-id text step-key opts]
  (let [row (observe h ref)]
    (send-text! h pane-id text (:vim-mode row))
    (step! state step-key {:sent text})
    ;; The turn may start and end between two polls; seeing busy is not required.
    (wait-until h ref :busy? opts (:start-timeout-ms opts))
    (or (wait-settled-idle h ref opts (:step-timeout-ms opts))
        (fail! state :timeout {:during step-key}))))

(defn compact!
  "Compact the pane REF names, through host H, recording progress in STATE
   (an atom). OPTS: see `defaults`, plus :force? (ignore the threshold) and
   :instructions (text after /compact). Returns STATE's final value."
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
                                 (fail! state :timeout {:during :wait-idle})))]
          (let [goal (when (and (:restore-goal? opts) (:goal-active? ready) (:session-id ready))
                       (panes/goal-text (or ((:transcript h) (:cwd ready) (:session-id ready)) [])))]
            (step! state :goal-captured {:goal goal :goal-active? (:goal-active? ready)})
            (swap! state assoc :goal goal)
            (when (and (:goal-active? ready) (:restore-goal? opts) (nil? goal))
              (step! state :goal-unreadable {:note "goal active but no /goal text in the transcript; it will not be restored"}))
            (when (send-and-settle! h state ref pane-id
                                    (str "/compact" (some->> (:instructions opts) str/trim not-empty (str " ")))
                                    :compact opts)
              (when (or (str/blank? (:reseed opts))
                        (send-and-settle! h state ref pane-id (:reseed opts) :reseed opts))
                (when (or (nil? goal)
                          (send-and-settle! h state ref pane-id (str "/goal " goal) :goal-restored opts))
                  (let [end (observe h ref)]
                    (swap! state assoc :status :done :tokens-after (:ctx-tokens end)
                           :goal-active-after? (:goal-active? end))))))))))
    @state))

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

(defn start!
  "Start a background compaction of the pane REF names. Returns the job map at
   once, or {:error ...} when that pane already has a running job. The job
   follows the pane by its tmux id, which /compact does not change. Pass :host
   in OPTS to use another port (tests); :sync? true runs in the caller."
  [ref {:keys [sync?] :as opts}]
  (let [h (or (:host opts) (host/host))
        row (observe h ref)]
    (if-let [running (and row (active-for (:pane row)))]
      {:error :already-running :job running}
      (let [job (new-job ref opts)
            state (atom (assoc job :pane (:pane row)))
            follow (or (:pane row) ref)]
        (swap! registry assoc (:job-id job) state)
        (let [work #(try (compact! h state follow (dissoc opts :host :sync?))
                         (catch Throwable t
                           (fail! state :threw {:message (.getMessage t)})
                           @state)
                         (finally (swap! state assoc :ended-at (System/currentTimeMillis))))]
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

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

   THE GOAL LOOP (card 20261122170735-7c6f5ad3). Measured on Claude Code
   2.1.330: a pane its /goal Stop hook keeps re-prompting shows NO rest at all
   (the spinner and its timer run on across turns, `running Stop hooks 1/2`
   at each turn end, then `Goal not yet met… continuing`), and a /compact
   typed into it stays Queued through every re-prompt. A force-sent
   (C-x C-s) /compact does run, but the goal stays active and the loop
   resumes. So on a pane `poll/goal-looping?` reads, the job reads the goal
   text, types `/goal clear` (taken in mid-turn, per the /goal docs and as
   measured), waits for the settled idle the ended loop gives, compacts,
   re-seeds with the goal text inside the re-seed prompt (`goal-reseed`), and
   sets the goal again. :goal-loop false turns it off.

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
            [hive-tmux.claude.interrupt :as interrupt]
            [hive-tmux.claude.panes :as panes]
            [hive-tmux.claude.poll :as poll]
            [hive-tmux.claude.screen :as screen]
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
   :drop-ratio 0.8
   ;; Cutting into a working turn (`hive-tmux.claude.interrupt`): :auto cuts
   ;; in only on a pane whose /goal was active when the job began, after
   ;; :interrupt-after-ms without rest, while a read-only wait (ss wait,
   ;; await, sleep) or no tool runs; true drops the goal and wait gates;
   ;; false never cuts in. Never mid-write, never over a draft.
   :interrupt :auto
   :interrupt-after-ms (* 5 60 1000)
   ;; :force-send queues /compact and sends it now with C-x C-s (the running
   ;; tool is moved to the background, not killed); :escape sends Esc until
   ;; the turn stops, then /compact.
   :interrupt-via :force-send
   :escape-wait-ms 3000
   ;; A pane its /goal Stop hook keeps re-prompting (`poll/goal-looping?`)
   ;; never rests, and a /compact typed into it stays Queued through every
   ;; re-prompt (Claude Code 2.1.330, measured). :auto clears the goal there
   ;; (`/goal clear` is taken in at once, even mid-turn), compacts once the
   ;; turn ends, re-seeds with the goal text in the prompt and sets the goal
   ;; again; false leaves such a pane to the other waits.
   :goal-loop :auto})

;;; Promote

(def idle?
  "Is ROW a Claude pane at rest? See `poll/idle?`."
  poll/idle?)

(defn due?
  "Should ROW be compacted at THRESHOLD tokens?"
  [row threshold]
  (boolean (some-> (:ctx-tokens row) (>= threshold))))

(defn open-dialog?
  "Is a dialog open on the pane of ROW? Its screen shows a menu or permission
   dialog (:dialog), or Claude Code's own session status is :waiting: the
   session waits on a human's answer. A :waiting pane shows no spinner, so it
   reads idle, and a job that trusted idle alone would type into the dialog."
  [row]
  (boolean (and row (or (:dialog row) (= :waiting (:status row))))))

(defn human-guard
  "Why nothing may be typed into the pane of ROW now, for a human's sake, or
   nil when nothing stops it (the human-draft guard, card
   20261122160537-60f33c7a):
     {:reason :open-dialog ...}            a dialog is open (`open-dialog?`)
     {:reason :human-typing :input text}   someone's text stands at the prompt
   Nothing lifts it: :force? skips a compaction's threshold, never a person,
   and an interrupt holds on a draft too (`interrupt/verdict`). A job either
   keeps waiting (queued, :waiting-on) or ends :skipped with this :reason."
  [row]
  (cond
    (open-dialog? row) (cond-> {:reason :open-dialog}
                         (:dialog row) (assoc :dialog (:dialog row))
                         (= :waiting (:status row)) (assoc :status :waiting))
    (:input row) {:reason :human-typing :input (:input row)}))

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
   it may change its status (`job-consistent?`, the state atom's validator,
   enforces it)."
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

(defn job-text
  "The text JOB would type, as a supervisor reads it: the prompt of a say,
   the /goal of a goal, the /compact (with instructions) of a compaction."
  [{:keys [kind opts]}]
  (case kind
    :say (str (:text opts))
    :goal (str "/goal " (or (:goal opts) (:goal-append opts)))
    (str "/compact" (some->> (:instructions opts) str/trim not-empty (str " ")))))

(defn blocking-view
  "What an :already-running refusal says of the BLOCKING job value: its id,
   kind, status, the first 80 characters of its text, and the step that
   typed, if one did."
  [job]
  (let [text (job-text job)]
    (cond-> {:job-id (:job-id job) :kind (:kind job :compact) :status (:status job)
             :text (subs text 0 (min 80 (count text)))}
      (:typed-step job) (assoc :typed-step (:typed-step job)))))

(defn start-refusal
  "Why a KIND job with OPTS would stop at once on the pane whose row is ROW,
   without typing: {:reason kw ...}, or nil when it would go on to work.
   The same cheap checks the runners make first (`compact!`, `start-row`,
   `say!`), so a supersede cancels the blocking job only for a job that will
   actually run."
  [kind opts row]
  (let [opts (merge defaults {:wait-idle? true} opts)]
    (cond
      (nil? row) {:reason :no-such-pane}
      (and (= :say kind) (nil? (some-> (:text opts) str str/trim not-empty))) {:reason :no-text}
      (and (= :compact kind) (not (:force? opts)) (not (due? row (:threshold opts))))
      {:reason :under-threshold :ctx-tokens (:ctx-tokens row) :threshold (:threshold opts)}
      (open-dialog? row) (human-guard row)
      ;; A say is never queued behind a draft (`say!`); compact and goal wait it out.
      (and (= :say kind) (:input row)) (human-guard row)
      ;; A /goal needs no rest (`goal!`), so only a say is refused as busy.
      (and (= :say kind) (false? (:wait-idle? opts)) (not (idle? row))) {:reason :busy})))

(defn unsent-after
  "The sends JOB planned after its /compact (:sends-planned, [{:step k
   :text t}]) that no step records as sent: [[k text] ...], in order."
  [job]
  (let [planned (:sends-planned job)
        ks (set (map :step planned))
        sent (set (keep #(when (and (ks (:step %)) (:sent %)) (:step %)) (:steps job)))]
    (vec (keep (fn [{:keys [step text]}] (when-not (sent step) [step text])) planned))))

(defn compacted-end
  "JOB, which recorded :compacted, stopped by a LATER step: :done all the
   same, since the compaction is what the job is for (friction
   20261121201539-4a5836cb). REASON says why the rest did not go out; STOP,
   what that step would have failed or skipped with, is kept under
   :advisory, never as an :error; LAST is the pane's screen state (else
   STOP's :last). The texts not typed are under :unsent, :reseed-pending and
   :goal-pending name which. A goal the pane shows active is not pending:
   it is in force."
  ([job reason stop] (compacted-end job reason stop nil))
  ([job reason stop last]
   (let [unsent (unsent-after job)
         ks (set (map first unsent))
         last (or last (:last stop))
         goal-active? (:goal-active? last)]
     (cond-> (-> job (assoc :status :done :reason reason) (dissoc :error))
       stop (assoc :advisory stop)
       (seq unsent) (assoc :unsent (mapv second unsent))
       (ks :reseed) (assoc :reseed-pending true)
       (and (ks :goal-restored) (not (:goal-in-force stop))) (assoc :goal-pending true)
       (some? goal-active?) (assoc :goal-active-after? goal-active?)
       last (assoc :last last)
       (:ctx-tokens last) (assoc :tokens-after (:ctx-tokens last))))))

(defn after-compact-reason
  "The :done reason of a compacted job that a later step stopped with REASON
   and DATA (`compacted-end`): a pane that stayed busy (:timeout or :busy)
   during step S reads :pane-busy-after-S; any other reason is kept."
  [reason {:keys [during]}]
  (if (and (#{:timeout :busy} reason) during)
    (keyword (str "pane-busy-after-" (name during)))
    reason))

;;; Pipeline

(defn- step! [state k data]
  (swap! state update :steps conj (assoc data :step k))
  (log/debug "[claude-compact]" k data))

(defn- stop!
  "End the job STATUS (:failed or :skipped) for REASON with DATA, unless it
   already recorded :compacted: then what stops it is advisory and it ends
   :done (`compacted-end`). Nil."
  [state status reason data]
  (if (:compacted? @state)
    (swap! state compacted-end (after-compact-reason reason data) (assoc data :reason reason))
    (swap! state assoc :status status :reason reason :error data))
  nil)

(defn- fail! [state reason data]
  (stop! state :failed reason data))

(defn- timeout!
  "Fail as :timeout DURING STEP-KEY, naming what the step waited for (DATA)
   and the pane's last screen state, so a supervisor can tell a slow pane from
   a stuck one and from a step that in fact succeeded."
  [h state ref step-key data]
  (fail! state :timeout (merge {:during step-key :last (screen-state (poll/observe h ref))} data)))

(defn goal-command?
  "Is TEXT a `/goal ...` command? Claude Code takes /goal in at once, even
   while a turn runs, so it is the one send that needs no rest."
  [text]
  (str/starts-with? (str text) "/goal "))

(defn goal-free?
  "May a /goal be typed into the pane of ROW now? It is a readable Claude
   prompt (a context count is shown), with no one's text in the composer and
   no dialog on screen. A working turn does not matter: /goal is accepted
   while busy (the pane's footer shows `/goal active (3s)` at once)."
  [row]
  (boolean (and row (some? (:ctx-tokens row)) (nil? (:input row)) (not (open-dialog? row)))))

(defn- dialog-stop!
  "Stop the job :skipped :open-dialog: a menu or permission dialog is on the
   pane, or its session is :waiting on a human (`open-dialog?`), so nothing is
   typed, and the dialog is reported instead of waited on (`tmux answer` picks
   an option). After the compaction it is advisory."
  [state pane-id row step-key]
  (stop! state :skipped :open-dialog
         (cond-> {:during step-key :pane pane-id :last (screen-state row)
                  :hint (str "A Claude Code menu or permission dialog is open on " pane-id
                             "; nothing was typed. Answer it (`tmux answer`), then start the job again.")}
           (:dialog row) (assoc :dialog (:dialog row))
           (= :waiting (:status row)) (assoc :status :waiting))))

(defn- deliver!
  "Type TEXT once the prompt is free (no one's text there, pane idle); true
   when it was submitted. Text someone left at the prompt is never typed over:
   it is waited out (`poll/wait-input-clear`, which also outlasts the turn it
   starts once sent) for at most :input-timeout-ms, while the job shows it
   under :waiting-on, and not at all when that is 0 or :wait-idle? is false.
   Still there, the job stops :skipped :human-typing naming the pane and the
   text; a pane that stayed busy stops it :skipped :busy; an open dialog
   (`open-dialog?`: on screen, or the session :waiting) stops it :skipped
   :open-dialog, never waited out and never typed into. Nothing in OPTS lifts
   this guard (`human-guard`): :force? is a compaction's threshold only.
   A slash command the composer does not hold back stops it :failed
   :composer-mismatch with Enter unpressed (`composer/send-text!`).
   The first keystroke is claimed on STATE (`claim-typing`) in the same swap
   a cancel would make, so a job cancelled before it types never types."
  [h state ref pane-id text step-key opts]
  (let [row0 (poll/observe h ref)
        goal-cmd? (goal-command? text)
        free? (if goal-cmd? (goal-free? row0) (and (poll/idle? row0) (nil? (human-guard row0))))
        wait-ms (if (false? (:wait-idle? opts)) 0 (or (:input-timeout-ms opts) 0))
        on (if (:input row0) {:human-typing (:input row0)} {:pane-busy true})
        row (cond (open-dialog? row0) nil
                  free? row0
                  (pos? wait-ms) (do (step! state :wait-input (assoc on :during step-key :timeout-ms wait-ms))
                                     (swap! state assoc :waiting-on (assoc on :during step-key))
                                     (try (if goal-cmd?
                                            (poll/wait-until h ref #(or (goal-free? %) (open-dialog? %)) opts wait-ms)
                                            (poll/wait-input-clear h ref opts wait-ms))
                                          (finally (swap! state dissoc :waiting-on)))))
        ;; The guard is read again on the row the job would type into.
        row (when-not (human-guard row) row)]
    (cond
      (cancelled? @state) false

      (and (not row) (open-dialog? (or (poll/observe h ref) row0)))
      (do (dialog-stop! state pane-id (or (poll/observe h ref) row0) step-key) false)

      (not row)
      (let [end (or (poll/observe h ref) row0)
            input (or (:input end) (:input row0))]
        ;; After the compaction this is advisory: the job ends :done (`stop!`).
        (stop! state :skipped (if input :human-typing :busy)
               (cond-> {:during step-key :pane pane-id :name (:name end)
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

(defn- reseed-and-settle!
  "Send the re-seed TEXT, an intermediate step, and wait for the turn it
   starts to end, so the next send does not land on a working pane.
   {:idle row} at rest. With GOAL-OK? (the goal the job
   would restore is the one the pane had), a pane that shows its /goal
   active ends the wait at once as {:goal-in-force row}: the goal is still
   set, its Stop hook keeps the pane working, and a turn boundary may never
   come (friction 20261121201539-4a5836cb). Nil when it stopped (status is
   set; after a compaction that is :done, see `stop!`)."
  [h state ref pane-id text goal-ok? opts]
  (when (deliver! h state ref pane-id text :reseed opts)
    ;; The turn may start and end between two polls; seeing busy is not required.
    (poll/wait-until h ref :busy? opts (:start-timeout-ms opts))
    (let [in-force? #(boolean (and goal-ok? (:goal-active? %)))
          row (poll/wait-rest h ref #(or (poll/idle? %) (in-force? %)) opts (:step-timeout-ms opts))]
      (cond (nil? row) (timeout! h state ref :reseed {:waited-for (poll/waited-for opts)})
            (poll/idle? row) {:idle row}
            :else {:goal-in-force row}))))

(defn- transcript-of
  "The transcript lines of ROW's session, or nil when they cannot be read."
  [h row]
  (when (:session-id row)
    (guard Exception nil (vec (or ((:transcript h) (:cwd row) (:session-id row)) [])))))

(defn- seconds-age?
  "Is AGE (the footer's `/goal active (3s)` text) a seconds-only age?"
  [age]
  (boolean (some->> age str (re-matches #"\d+s"))))

(defn goal-confirmed?
  "Is GOAL proven set on ROW after the /goal send? The footer must show a
   goal active AND it must be the goal just sent: a /goal entry for it in the
   transcript after the BEFORE-COUNT lines there were at the send, or a footer
   age back to seconds where BEFORE-ROW showed no seconds-old goal (a busy
   pane may write the entry only when its turn ends). A footer that was
   already there proves nothing, and a busy pane is not an acknowledgement."
  [goal before-row before-count row lines]
  (boolean
   (and (:goal-active? row)
        (or ;; No goal was shown before the send: the footer appearing is
            ;; this goal (a busy pane may not yet have written its entry).
            (and before-row (not (:goal-active? before-row)))
            (and before-count (> (count lines) before-count)
                 (= goal (panes/goal-text (subvec (vec lines) before-count))))
            (and (seconds-age? (:goal-age row))
                 (not (and (:goal-active? before-row) (seconds-age? (:goal-age before-row)))))))))

(defn- send-and-start!
  "Send TEXT as a job's LAST step and end on the pane's acknowledgement: seen
   working, or, for a /goal, the goal shown active, within :start-timeout-ms.
   Waiting for idle here would wait on the very work the send restarts.
   {:started? bool :row row}; an unacknowledged delivery is :started? false,
   not a hang. Nil when the delivery stopped it (status is set)."
  [h state ref pane-id text step-key opts]
  (let [goal (when (goal-command? text) (subs text 6))
        before (when goal (poll/observe h ref))
        n0 (when goal (some-> (transcript-of h before) count))]
    (when (deliver! h state ref pane-id text step-key opts)
      (let [ack (if goal
                  #(goal-confirmed? goal before n0 % (or (transcript-of h %) []))
                  #(boolean (:busy? %)))
            seen (poll/wait-until h ref ack opts (:start-timeout-ms opts))]
        {:started? (boolean seen) :row (or seen (poll/observe h ref))}))))

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
   stopped (status is set).

   SEND!, a thunk answering truthy once TEXT went out, replaces the normal
   delivery (`deliver!`, which waits for a free prompt): the force-send
   interrupt types into a working pane on purpose (`force-send!`)."
  ([h state ref pane-id text before opts]
   (send-and-compact! h state ref pane-id text before opts
                      #(deliver! h state ref pane-id text :compact opts)))
  ([h state ref pane-id text before opts send!]
   (let [{:keys [now-ms sleep!]} h
         n0 (some-> (boundaries-of h (poll/observe h ref)) count)]
     (when (send!)
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
                :row (poll/wait-rest h ref poll/idle? opts (:step-timeout-ms opts))})))))))

(defn- interrupt-gate
  "Wait for rest like `poll/wait-rest`, but read the pane every :poll-ms and
   ask `interrupt/verdict` whether the job may cut in instead. {:ready row}
   at rest, {:interrupt verdict :row row} once allowed, nil at
   :idle-timeout-ms. A changed hold reason is recorded as an :interrupt-hold
   step and shown under :interrupt-hold, so a supervisor sees why it waits."
  [h state ref pane-id start opts]
  (let [{:keys [now-ms]} h
        t0 (now-ms)
        deadline (+ t0 (:idle-timeout-ms opts))
        goal? (boolean (:goal-active? start))]
    (loop [last-hold nil]
      (if-let [row (poll/wait-rest h ref poll/idle? opts
                                   (min (:poll-ms opts) (max 0 (- deadline (now-ms)))))]
        (do (swap! state dissoc :interrupt-hold) {:ready row})
        (let [row (poll/observe h ref)
              tool (some-> (when row (panes/capture h pane-id 40)) interrupt/running-tool)
              v (interrupt/verdict opts {:row row :tool tool :waited-ms (- (now-ms) t0)
                                         :goal-at-start? goal?})]
          (cond
            ;; Seen re-prompting by its Stop hook: no rest will come, and a
            ;; forced /compact leaves the goal running (`compact!` clears it).
            (and (:detect-goal-loop? opts) (poll/goal-looping? row))
            (do (swap! state dissoc :interrupt-hold) {:goal-loop row})

            (:interrupt? v) (do (swap! state dissoc :interrupt-hold) {:interrupt v :row row})
            (>= (now-ms) deadline) nil
            :else (do (when (not= (:hold v) last-hold)
                        (step! state :interrupt-hold (select-keys v [:hold :tool]))
                        (swap! state assoc :interrupt-hold (select-keys v [:hold :tool])))
                      (recur (:hold v)))))))))

(defn- escape-stop!
  "Stop the running turn of PANE-ID with Esc, at most three, :escape-wait-ms
   apart (in vim INSERT mode the first Esc only leaves insert mode). The idle
   row, or nil when the pane kept working (:failed :interrupt-failed) or the
   job was cancelled. The Esc is the job's first keystroke (`claim-typing`)."
  [{:keys [tmux! sleep!] :as h} state ref pane-id verdict opts]
  (when-not (cancelled? (swap! state claim-typing :interrupt))
    (loop [n 0]
      (let [row (poll/observe h ref)]
        (cond
          (and row (not (:busy? row)))
          (do (step! state :interrupt {:via :escape :escapes n :tool (get-in verdict [:tool :tool])})
              (swap! state assoc :interrupted-via :escape)
              row)

          (>= n 3)
          (fail! state :interrupt-failed {:via :escape :escapes n :last (screen-state row)})

          :else
          (do (tmux! ["send-keys" "-t" pane-id "Escape"])
              (sleep! (:escape-wait-ms opts))
              (recur (inc n))))))))

(defn- queue-now
  "What waits in the queue of PANE-ID now: {:queued [text ...] :hint? bool},
   :hint? when the composer shows `screen/queue-hint`."
  [h pane-id]
  (let [s (screen/read-screen (or (panes/capture h pane-id 40) []))]
    {:queued (or (:queued s) []) :hint? (= screen/queue-hint (:suggestion s))}))

(def ^:private flush-reads
  "Reads, 300 ms apart, a flush waits for the queue to empty. Claude Code
   takes a queued message in at the turn's next model call, which can come
   later; the job goes on anyway, since /compact queues behind it."
  10)

(defn- flush-queued!
  "Send now, with C-x C-s, the messages someone SUBMITTED to the working
   PANE-ID that wait in Claude Code's queue (`screen/queued-marker`): they are
   that person's intent, delivered before /compact and never dropped. Each is
   recorded under :human-sent and as a :human-sent step. The texts sent;
   nil when nothing was queued."
  [{:keys [tmux! sleep!] :as h} state pane-id]
  (let [{:keys [queued hint?]} (queue-now h pane-id)]
    (when (or (seq queued) hint?)
      (tmux! ["send-keys" "-t" pane-id "C-x" "C-s"])
      (step! state :human-sent {:texts queued :via :force-send})
      (swap! state update :human-sent (fnil into []) queued)
      (loop [n flush-reads]
        (sleep! 300)
        (let [q (queue-now h pane-id)]
          (when (and (pos? n) (or (seq (:queued q)) (:hint? q)))
            (recur (dec n)))))
      queued)))

(defn- force-send!
  "Type the /compact TEXT into the WORKING PANE-ID, where Claude Code queues
   it (`Queued · ctrl+x ctrl+s to send now`), then send it now with C-x C-s:
   the running tool is moved to the background, not killed, and the
   compaction starts. True once sent; false when the job was cancelled or
   the composer did not hold TEXT (:failed :composer-mismatch, Enter unpressed).

   A message someone already SUBMITTED that waits in the queue is not a
   draft: it is sent first (`flush-queued!`), then /compact is typed. An
   unsent draft never gets here: `interrupt/verdict` holds on it."
  [{:keys [tmux! sleep!] :as h} state pane-id text row verdict]
  (if (cancelled? (swap! state claim-typing :interrupt))
    false
    (let [flushed (flush-queued! h state pane-id)
          ;; A /compact already in the queue (an earlier attempt) went out
          ;; with the flush: typing a second one would compact twice.
          already (some #(when (composer/holds? text %) %) flushed)
          {:keys [submitted? composer]}
          (if already
            {:submitted? true}
            (do (tmux! ["send-keys" "-t" pane-id "C-End"])
                (composer/send-text! h pane-id text (:vim-mode row))))]
      (cond
        already
        (do (step! state :interrupt {:via :force-send :tool (get-in verdict [:tool :tool])})
            (step! state :compact {:sent already :was-queued true})
            (swap! state assoc :interrupted-via :force-send)
            true)

        (not submitted?)
        (boolean (fail! state :composer-mismatch
                        (cond-> {:during :interrupt :pane pane-id :expected text :composer composer}
                          (seq (:human-sent @state)) (assoc :human-sent (:human-sent @state)))))

        :else
        (do (sleep! 500)
            (tmux! ["send-keys" "-t" pane-id "C-x" "C-s"])
            (step! state :interrupt {:via :force-send :tool (get-in verdict [:tool :tool])})
            (step! state :compact {:sent text :queued-then-forced true})
            (swap! state assoc :interrupted-via :force-send)
            true)))))

(defn goal-after
  "The goal to set after the re-seed. GOAL (an override) wins over CAPTURED;
   APPEND is added to whichever applies. Nil when there is nothing to set."
  [{:keys [goal goal-append]} captured]
  (let [base (or (some-> goal str/trim not-empty) captured)
        extra (some-> goal-append str/trim not-empty)]
    (cond (and base extra) (str base " " extra)
          :else (or base extra))))

(defn- clear-goal!
  "Type `/goal clear` into the (busy) PANE-ID and wait for its footer to drop
   the goal, up to :idle-timeout-ms: Claude Code may act on it only when the
   running tool call returns (review finding 9). True once cleared; nil when
   it stopped (status is set), after sending the goal back if it was cleared
   late. The goal captured before is under :cleared-goal."
  [h state ref pane-id opts]
  (when (deliver! h state ref pane-id "/goal clear" :goal-clear opts)
    (swap! state assoc :goal-cleared? true)
    (or (boolean (poll/wait-until h ref #(and % (not (:goal-active? %))) opts (:idle-timeout-ms opts)))
        (do (timeout! h state ref :goal-clear {:waited-for :goal-cleared}) nil))))

(defn goal-loop-on?
  "Does OPTS let a job take the goal-loop path (`:goal-loop`, default :auto)?"
  [opts]
  (not (false? (:goal-loop opts))))

(defn goal-reseed
  "The re-seed RESEED of a job that cleared a looping /goal to compact the
   pane, carrying the GOAL text, so the session reads what it was working
   toward even before the /goal is set again (the next send). RESEED alone
   when there is no goal; nil when there is no re-seed."
  [reseed goal]
  (when-not (str/blank? reseed)
    (if (str/blank? goal)
      reseed
      (str reseed " -- then resume your /goal, paused for this compaction and set again right after: " goal))))

(defn- goal-loop-clear!
  "The goal-loop path (card 20261122170735-7c6f5ad3) on ROW, a pane whose
   /goal Stop hook keeps re-prompting (`poll/goal-looping?`): read the goal
   text (transcript, else OPTS :goal), type `/goal clear` (taken in mid-turn;
   a draft is waited out, `deliver!`), then wait for the settled idle the
   ended loop now gives. {:ready row :pre-captured goal :cleared? true}, or
   nil when it stopped (status is set). A goal whose text cannot be read is
   never cleared: the job stops :skipped :goal-unreadable, nothing typed."
  [h state ref pane-id row opts]
  (let [goal (or (some-> (transcript-of h row) panes/goal-text) (some-> (:goal opts) str str/trim not-empty))]
    (step! state :goal-loop (cond-> {:goal goal}
                              (:stop-hook-running? row) (assoc :stop-hook-running? true)
                              (:goal-continuing? row) (assoc :goal-continuing? true)))
    (swap! state assoc :goal-loop? true)
    (if-not goal
      (stop! state :skipped :goal-unreadable
             {:during :goal-loop :pane pane-id :last (screen-state row)
              :hint (str "The /goal of " pane-id " loops on its Stop hook, but its text is in no transcript entry, "
                         "so it was not cleared (it could not be restored). Pass goal=<text> to compact it.")})
      (do (swap! state assoc :goal (goal-after opts (when (:restore-goal? opts) goal)))
          (when (clear-goal! h state ref pane-id opts)
            (step! state :wait-idle {:after :goal-clear})
            (if-let [r (poll/wait-rest h ref poll/idle? (assoc opts :between-turns? false) (:idle-timeout-ms opts))]
              {:ready r :pre-captured goal :cleared? true}
              (timeout! h state ref :wait-idle {:waited-for :settled-idle :after :goal-clear})))))))

(defn- goal-cleared-unrestored?
  "Did JOB type `/goal clear` and end without a confirmed `/goal` restore?"
  [job]
  (let [sent (set (keep #(when (:sent %) (:step %)) (:steps job)))]
    (boolean (and (:goal-cleared? job) (some? (:goal job)) (not (sent :goal-restored))))))

(defn- restore-cleared-goal!
  "Send back the goal a clear_goal job cleared when the job stopped short of
   restoring it (a timeout, an unacknowledged re-seed, a busy pane, a cancel),
   so the pane is never left goalless and its peer does not stop. /goal is
   accepted while busy; a draft is waited out, a dialog refuses. It runs on a
   private job map, so a cancel does not block it, and it is recorded under
   :restore and in the steps, never as the job's outcome."
  [h state ref opts]
  (let [goal (:goal @state)
        probe (atom {:status :running :steps []})
        sent (guard Throwable {:threw true}
               (send-and-start! h probe ref (:pane @state) (str "/goal " goal) :goal-restored
                                (assoc opts :wait-idle? true)))]
    (swap! state (fn [job]
                   (-> job
                       (update :steps into (:steps @probe))
                       (assoc :restore (cond (:started? sent) {:restored? true :goal goal}
                                             sent {:restored? :unconfirmed :goal goal}
                                             :else {:restored? false :goal goal
                                                    :why (select-keys @probe [:reason :error])})))))))

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

   ONCE :compacted IS RECORDED THE JOB CANNOT FAIL. Every later wait for rest
   is advisory (`stop!`, `compacted-end`): a pane still busy after the
   re-seed, because the /goal is still in force and its Stop hook leaves no turn
   boundary, ends :done :pane-busy-after-reseed. What the step would have
   failed with is kept under :advisory, the texts not typed go under :unsent,
   and :reseed-pending / :goal-pending say which (friction
   20261121201539-4a5836cb: cc-04c4ee28 and cc-804a41e1 compacted 463k ->
   151k, then ended `failed timeout`). A job that never compacted still
   fails as before.

   REST: see the namespace docstring. Nothing here lands on a running turn."
  [h state ref opts]
  (let [opts (-> (merge defaults opts)
                 (update :interrupt interrupt/mode-param)
                 (update :interrupt-via interrupt/via-param))
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

      ;; :force? lifts the threshold above, never this guard.
      (open-dialog? start)
      (dialog-stop! state (:pane start) start :start)

      :else
      (let [pane-id (:pane start)
            ;; clear_goal: read the goal while it is still active, then type
            ;; `/goal clear` into the busy pane so its Stop hook lets the turn
            ;; end; from there rest is a settled idle as on any pane.
            pre-captured (when (and (:clear-goal? opts) (:goal-active? start))
                           (or (some-> (transcript-of h start) panes/goal-text) (:goal opts)))
            cleared? (when pre-captured
                       ;; Known before the first key, so a clear that times out
                       ;; still sends this goal back (`restore-cleared-goal!`).
                       (swap! state assoc :goal (goal-after opts (when (:restore-goal? opts) pre-captured)))
                       (clear-goal! h state ref pane-id opts))
            opts (cond-> opts cleared? (assoc :between-turns? false))]
        (when-let [gate (when (or (nil? pre-captured) cleared?)
                          (let [;; Goal-loop detection runs only on a pane whose /goal is
                                ;; active at the start and not already cleared: every other
                                ;; pane takes exactly the waits it took before.
                                detect? (and (goal-loop-on? opts) (nil? pre-captured) (:goal-active? start))
                                gate (if (and detect? (poll/goal-looping? start))
                                       {:goal-loop start}
                                       (do (step! state :wait-idle {:interrupt (:interrupt opts)})
                                           (or (if (and (false? (:interrupt opts)) (not detect?))
                                                 (some->> (poll/wait-rest h ref poll/idle? opts (:idle-timeout-ms opts))
                                                          (hash-map :ready))
                                                 (interrupt-gate h state ref pane-id start
                                                                 (assoc opts :detect-goal-loop? detect?)))
                                               (timeout! h state ref :wait-idle (cond-> {:waited-for (poll/waited-for opts)}
                                                                                  (:interrupt-hold @state)
                                                                                  (assoc :interrupt-hold (:interrupt-hold @state)))))))]
                            (if-let [row (:goal-loop gate)]
                              (goal-loop-clear! h state ref pane-id row opts)
                              gate)))]
          (let [;; The goal-loop path cleared the goal itself: from here on the
                ;; job is the clear_goal job, its goal text captured before the clear.
                pre-captured (or (:pre-captured gate) pre-captured)
                looped? (boolean (:cleared? gate))
                cleared? (or looped? cleared?)
                opts (cond-> opts looped? (assoc :between-turns? false))
                ready (or (:ready gate) (:row gate))
                verdict (:interrupt gate)
                captured (if cleared?
                           (when (:restore-goal? opts) pre-captured)
                           (when (and (:restore-goal? opts) (:goal-active? ready) (:session-id ready))
                             (panes/goal-text (or ((:transcript h) (:cwd ready) (:session-id ready)) []))))
                goal (goal-after opts captured)
                reseed (if looped?
                         ;; The goal text rides in the re-seed too (`goal-reseed`).
                         (goal-reseed (:reseed opts) goal)
                         (when-not (str/blank? (:reseed opts)) (:reseed opts)))
                sends (cond-> [] reseed (conj [reseed :reseed]) goal (conj [(str "/goal " goal) :goal-restored]))
                text (str "/compact" (some->> (:instructions opts) str/trim not-empty (str " ")))
                ;; The goal is read while the turn still runs, before any key.
                _ (do (step! state :goal-captured {:captured captured :goal goal
                                                   :goal-active? (:goal-active? ready)})
                      (swap! state assoc :goal goal
                             :sends-planned (mapv (fn [[t k]] {:step k :text t}) sends))
                      (when (and (:goal-active? ready) (:restore-goal? opts) (nil? captured) (nil? (:goal opts)))
                        (let [note (str "The /goal is active but its text is in neither a /goal entry nor a goal_status "
                                        "entry of the session transcript, so it will NOT be restored after /compact. "
                                        "The pane is compacted anyway; set it again with `tmux goal pane=" pane-id " goal=...`.")]
                          (step! state :goal-unreadable {:note note})
                          (swap! state assoc :goal-unreadable note))))
                ;; Esc would cut the turn that takes a queued human message in,
                ;; so with one waiting the interrupt is a force-send instead.
                opts (if (and verdict (= :escape (:interrupt-via opts))
                              (seq (:queued (queue-now h pane-id))))
                       (do (step! state :interrupt-via {:via :force-send :why :human-message-queued})
                           (assoc opts :interrupt-via :force-send))
                       opts)
                escaped (when (and verdict (= :escape (:interrupt-via opts)))
                          (escape-stop! h state ref pane-id verdict opts))]
            (when-let [{:keys [row]} (cond
                                       ;; A /compact already QUEUED on the looping pane (an
                                       ;; earlier job's force-send, or a human's) ran when
                                       ;; the cleared loop let the turn end (friction
                                       ;; 220d00ce): the context fell from where the job
                                       ;; found it. A second /compact is not typed.
                                       (and looped? (ctx-drop (:ctx-tokens start) ready (:drop-ratio opts)))
                                       (send-and-compact! h state ref pane-id text (:ctx-tokens start) opts
                                                          #(do (step! state :compact {:was-queued true :ran-at-goal-clear true})
                                                               true))

                                       (nil? verdict)
                                       (send-and-compact! h state ref pane-id text (:ctx-tokens ready) opts)

                                       (= :escape (:interrupt-via opts))
                                       (when escaped
                                         (send-and-compact! h state ref pane-id text (:ctx-tokens ready) opts))

                                       :else
                                       (send-and-compact! h state ref pane-id text (:ctx-tokens ready) opts
                                                          #(force-send! h state pane-id text ready verdict)))]
              (cond
                (and (nil? row) (seq sends))
                (swap! state compacted-end :pane-busy-after-compact nil (screen-state (poll/observe h ref)))

                ;; The last send restarts work: it ends the job once acknowledged.
                :else
                (let [last-send (peek sends)
                      settled (when (>= (count sends) 2)
                                (if cleared?
                                  ;; The goal was cleared on purpose and /goal is
                                  ;; accepted while busy: the re-seed only has to
                                  ;; be acknowledged (seen working), never rested.
                                  (when-let [r (send-and-start! h state ref pane-id reseed :reseed opts)]
                                    (swap! state assoc :reseed-acknowledged? (:started? r))
                                    {:idle (:row r)})
                                  (reseed-and-settle! h state ref pane-id reseed (= goal captured) opts)))]
                  (cond
                    ;; The goal outlived /compact and keeps the pane working:
                    ;; nothing is left to restore, and nothing is typed into it.
                    (:goal-in-force settled)
                    (swap! state compacted-end :pane-busy-after-reseed
                           {:during :reseed :waited-for (poll/waited-for opts) :goal-in-force true}
                           (screen-state (:goal-in-force settled)))

                    (and (>= (count sends) 2) (nil? settled)) nil

                    last-send
                    (let [[text k] last-send]
                      (when-let [{:keys [started? row]} (send-and-start! h state ref pane-id text k opts)]
                        (swap! state assoc :status :done :started? started?
                               :tokens-after (:ctx-tokens row) :goal-active-after? (:goal-active? row))))

                    :else
                    (let [end (poll/observe h ref)]
                      (swap! state assoc :status :done :tokens-after (:ctx-tokens end)
                             :goal-active-after? (:goal-active? end)))))))))))
    (when (goal-cleared-unrestored? @state)
      (restore-cleared-goal! h state ref opts))
    @state))

(defn- start-row
  "The pane's first row, waited to a settled idle when WAIT-IDLE? (else it
   must already be idle). Nil when it stopped (status is set)."
  [h state ref {:keys [wait-idle? goal-command?] :as opts}]
  (let [start (poll/observe h ref)
        opts (poll/with-turns opts start)]
    (swap! state assoc :status :running :pane (:pane start) :name (:name start)
           :tokens-before (:ctx-tokens start) :between-turns? (:between-turns? opts))
    (cond
      (nil? start) (fail! state :no-such-pane {:ref ref})
      ;; A dialog is reported at once, never waited out as if it were work.
      (open-dialog? start) (dialog-stop! state (:pane start) start :start)
      ;; A say is not queued behind a draft: refused at once, no idle wait first.
      (and (:refuse-input? opts) (:input start))
      (stop! state :skipped :human-typing
             {:during :start :pane (:pane start) :name (:name start) :input (:input start)
              :last (screen-state start)
              :hint (str "Text stands at the prompt of " (:pane start) " and was not typed over. "
                         "Send or clear it, then start the job again.")})
      (nil? (:ctx-tokens start))
      (stop! state :skipped :unreadable-screen
             {:pane (:pane start) :last (screen-state start)
              :hint "The pane shows no readable Claude Code prompt and status line; nothing was typed."})
      ;; A /goal is accepted while a turn runs: no wait for an idle that a
      ;; goal's Stop hook may never allow (card 20261119233707-70541f83).
      goal-command? start
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
      (when-let [ready (start-row h state ref (assoc opts :refuse-input? true))]
        (when (deliver! h state ref (:pane ready) text :say (assoc opts :input-timeout-ms 0))
          (let [started (poll/wait-until h ref :busy? opts (:start-timeout-ms opts))]
            (swap! state assoc :status :done :delivered true
                   :started? (boolean started)))))))
  @state)

(defn goal!
  "Set the /goal of the pane REF names without compacting it: OPTS :goal
   replaces, :goal-append extends the pane's active goal (read from its
   transcript). Claude Code takes /goal in while a turn runs, so the job
   never waits for an idle (a busy /goal pane may never give one); a dialog
   or an unreadable screen stops it, and text at the prompt is waited out up
   to :input-timeout-ms (`deliver!`). :done only once the footer shows THIS
   goal active (`goal-confirmed?`); else :failed :goal-unconfirmed."
  [h state ref opts]
  (let [opts (merge defaults {:wait-idle? true :goal-command? true} opts)]
    (when-let [ready (start-row h state ref opts)]
      (let [captured (when (and (:goal-active? ready) (:session-id ready))
                       (panes/goal-text (or ((:transcript h) (:cwd ready) (:session-id ready)) [])))
            goal (goal-after opts captured)]
        (swap! state assoc :goal goal :captured captured)
        (if-not goal
          (fail! state :no-goal {:note "pass goal, or goal_append on a pane whose goal is active"})
          (when-let [{:keys [started? row]} (send-and-start! h state ref (:pane ready) (str "/goal " goal) :goal-set opts)]
            (if started?
              (swap! state assoc :status :done :started? true :goal-active-after? (:goal-active? row))
              (fail! state :goal-unconfirmed
                     {:during :goal-set :goal goal :last (screen-state row)
                      :hint "The /goal was submitted but the footer never showed this goal active."})))))))
  @state)

;;; Boundary

(defonce ^:private registry (atom {}))

(defn record-step!
  "Append an external STEP to the active job on PANE-ID, without starting or
   displacing it. Returns the updated job, or nil when the pane has no active
   job. Used by menu answers, which do not own the pane's job slot."
  [pane-id step]
  (when-let [state (some (fn [[_ state]]
                           (let [job @state]
                             (when (and (= pane-id (:pane job))
                                        (#{:queued :running} (:status job)))
                               state)))
                         @registry)]
    (swap! state update :steps conj step)))

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
   its next wait (`cancellable-host`), and :on-end runs once, from `cancel!`.

   :already-running names the blocking job (`blocking-view`) and hints
   supersede. OPTS :supersede? true cancels that job (reason :superseded) when
   it has not typed, and takes the slot; one that typed refuses :already-typed
   naming the step, and nothing is started."
  [ref {:keys [sync? on-end supersede?] :as opts}]
  (let [h (or (:host opts) (host/host))
        kind (or (:kind opts) :compact)
        runner (get runners kind)
        row (poll/observe h ref)
        blocking (when (and runner row) (active-for (:pane row)))
        ;; Cheap pre-checks first: a supersede cancels only for a job that will run.
        doomed (when (and blocking supersede?) (start-refusal kind (dissoc opts :host) row))
        superseded (when (and blocking supersede? (not doomed)) (cancel! (:job-id blocking) :superseded))]
    (cond
      (nil? runner) {:error :unknown-kind :kind kind}

      doomed
      {:error :would-not-start :refusal doomed :blocking (blocking-view blocking)
       :hint (str "The new " (name kind) " job would stop at once (" (name (:reason doomed)) "), so job "
                  (:job-id blocking) " was not superseded.")}

      (and blocking (not supersede?))
      {:error :already-running :job blocking :blocking (blocking-view blocking)
       :hint (str "Pane " (:pane row) " holds " (name (:kind blocking :compact)) " job " (:job-id blocking)
                  ". supersede=true cancels it, if it has not typed yet, and takes the slot; or `swarm tmux job job_id="
                  (:job-id blocking) " cancel=true`.")}

      (= :already-typed (:refuse superseded))
      {:error :already-typed :step (:step superseded) :blocking (blocking-view (:job superseded))
       :hint (str "Job " (:job-id blocking) " already typed its " (name (:step superseded))
                  " step into pane " (:pane row) "; it is not superseded. Wait for it to end.")}

      :else
      (let [job (cond-> (assoc (new-job ref (dissoc opts :on-end :supersede?)) :kind kind)
                  (:cancelled? superseded) (assoc :superseded (:job-id blocking)))
            state (atom (assoc job :pane (:pane row))
                        :meta {:on-end on-end}
                        :validator job-consistent?)
            follow (or (:pane row) ref)
            h (cancellable-host h state)]
        (swap! registry assoc (:job-id job) state)
        (let [work #(try (runner h state follow (dissoc opts :host :sync? :kind :on-end :supersede?))
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

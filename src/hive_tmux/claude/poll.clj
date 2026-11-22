(ns hive-tmux.claude.poll
  "Waiting on a Claude Code pane: poll its row until a predicate holds, until it
   is at rest twice in a row, until its prompt is free of anyone's text.

   Two ways to wait for rest, and the difference matters:

     settled idle   idle twice, SETTLE-MS apart. A pane between two tool calls
                    looks idle for a moment, so one idle read proves nothing.
     turn boundary  idle once. Under a /goal Stop hook the pane starts its next
                    turn within one poll, so the rest between turns lasts a
                    single read and never two in a row: waiting for two can
                    only ever time out (friction 20261116185504-0376516a).

   `:between-turns?` in the opts selects the second; every waiter here honours
   it, so a job stays consistent across all of its steps.

   Stratum: Pipeline, under `hive-tmux.claude.compact`. Every read, clock and
   sleep goes through the host port, so a test drives time."
  (:require [hive-dsl.result :refer [guard]]
            [hive-tmux.claude.panes :as panes]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn idle?
  "Is ROW a Claude pane at rest? The screen shows no spinner and Claude Code's
   own session status, when known, is not busy. A row that could not be read
   is not idle."
  [row]
  (boolean (and row (some? (:ctx-tokens row)) (not (:busy? row)))))

(defn turn-boundary?
  "Is ROW a turn boundary: the pane is at rest, so whatever turn it was running
   has ended -- whether or not a hook starts the next one straight away. Read
   once, with no second reading to confirm: see the namespace docstring."
  [row]
  (idle? row))

(defn goal-looping?
  "Is ROW a pane its /goal Stop hook keeps re-prompting (card
   20261122170735-7c6f5ad3)? Its goal is active and the screen shows the loop
   itself: the spinner says `running Stop hooks`, or the evaluator's `Goal not
   yet met… continuing` line stands above it. Measured on Claude Code 2.1.330:
   such a pane shows no idle read at all (the spinner and its timer run on
   across turns), and a /compact typed into it stays Queued through every
   re-prompt, so waiting for rest can only time out."
  [row]
  (boolean (and row (:goal-active? row) (:busy? row)
                (or (:stop-hook-running? row) (:goal-continuing? row)))))

(defn observe
  "The row of the pane REF names, read through host H; nil when it cannot be
   read."
  [h ref]
  (guard Exception nil (panes/pane h ref)))

(defn wait-until
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

(defn wait-settled
  "PRED holds twice, SETTLE-MS apart: a pane between two tool calls looks idle
   for a moment. The second row, or nil on timeout."
  [h ref pred {:keys [settle-ms] :as opts} timeout-ms]
  (let [{:keys [now-ms sleep!]} h
        deadline (+ (now-ms) timeout-ms)]
    (loop []
      (when-let [first-ok (wait-until h ref pred opts (max 0 (- deadline (now-ms))))]
        (sleep! settle-ms)
        (let [row (observe h ref)]
          (cond (pred row) (or row first-ok)
                (>= (now-ms) deadline) nil
                :else (recur)))))))

(defn wait-settled-idle
  "Idle twice, SETTLE-MS apart. The second row, or nil on timeout."
  [h ref opts timeout-ms]
  (wait-settled h ref idle? opts timeout-ms))

(defn wait-turn-boundary
  "The first row that is at rest: the boundary between two turns. One read is
   the whole proof, because a /goal Stop hook leaves no more than one."
  [h ref opts timeout-ms]
  (wait-until h ref turn-boundary? opts timeout-ms))

(defn turns?
  "Is rest for this job proven at a single turn boundary? OPTS :between-turns?
   true or false forces it; :auto (the default) asks ROW: a pane whose /goal is
   active is restarted by a Stop hook the moment its turn ends, so its rest
   lasts one read. A row that cannot be read is not evidence of a hook."
  [opts row]
  (let [v (get opts :between-turns? :auto)]
    (if (= :auto v) (boolean (:goal-active? row)) (boolean v))))

(defn with-turns
  "OPTS with :between-turns? resolved to a boolean against the job's FIRST row.
   Resolved once per job, because the evidence that picks it (an active /goal)
   is gone after /compact clears it, and a job must not change how it proves
   rest halfway through."
  [opts row]
  (assoc opts :between-turns? (turns? opts row)))

(defn waited-for
  "What a wait on rest waits for, named for the timeout error: :turn-boundary
   when the job reads a single rest (a /goal pane between turns), :settled-idle
   when it needs two."
  [opts]
  (if (:between-turns? opts) :turn-boundary :settled-idle))

(defn wait-rest
  "The row where PRED holds and the pane is confirmed at rest, in the way OPTS
   ask for: a single read when :between-turns? is literally true (a turn
   boundary), two reads SETTLE-MS apart otherwise (a settled idle). Anything
   else -- absent, nil, or an unresolved :auto -- means a settled idle, since
   that is the contract every caller had before turn boundaries existed.
   Nil on timeout."
  [h ref pred opts timeout-ms]
  (if (true? (:between-turns? opts))
    (wait-until h ref pred opts timeout-ms)
    (wait-settled h ref pred opts timeout-ms)))

(defn wait-input-clear
  "Poll until the prompt holds no one's text and the pane is at rest, at most
   TIMEOUT-MS: text that was sent starts a turn, and that turn must end before
   anything else is typed. The row, or nil on timeout."
  [h ref opts timeout-ms]
  (let [{:keys [now-ms]} h
        deadline (+ (now-ms) timeout-ms)
        left #(max 0 (- deadline (now-ms)))
        free? #(and (idle? %) (nil? (:input %)))]
    (loop []
      (when (wait-until h ref free? opts (left))
        (let [row (wait-rest h ref free? opts (left))]
          (cond (free? row) row
                (pos? (left)) (recur)
                :else nil))))))

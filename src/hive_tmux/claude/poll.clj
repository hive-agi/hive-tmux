(ns hive-tmux.claude.poll
  "Waiting on a Claude Code pane: poll its row until a predicate holds, until
   it is idle twice in a row, until its prompt is free of anyone's text.

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

(defn wait-settled-idle
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

(defn wait-input-clear
  "Poll until the prompt holds no one's text and the pane is at a settled
   idle, at most TIMEOUT-MS: text that was sent starts a turn, and that turn
   must end before anything else is typed. The row, or nil on timeout."
  [h ref opts timeout-ms]
  (let [{:keys [now-ms]} h
        deadline (+ (now-ms) timeout-ms)
        left #(max 0 (- deadline (now-ms)))
        free? #(and (idle? %) (nil? (:input %)))]
    (loop []
      (when (wait-until h ref free? opts (left))
        (let [row (wait-settled-idle h ref opts (left))]
          (cond (free? row) row
                (pos? (left)) (recur)
                :else nil))))))

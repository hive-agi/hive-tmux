(ns hive-tmux.events
  "Event registrations and fx handlers for tmux pane lifecycle.

   Events:
     :tmux/pane-created       — after pane is registered in state
     :tmux/pane-killed        — after pane is killed and deregistered
     :tmux/task-dispatched    — after task sent to pane
     :tmux/interrupt-sent     — after C-c sent to pane
     :tmux/state-changed      — after registry mutation

   Effects:
     :tmux/log                — log event via timbre"
  (:require [hive.events :as ev]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Initialization
;;; =============================================================================

(defn ensure-init!
  "Ensure the event system is initialized. Idempotent — safe to call multiple times."
  []
  nil)

;;; =============================================================================
;;; Effect Handlers
;;; =============================================================================

(ev/reg-fx :tmux/log
           (fn [{:keys [level msg data]}]
             (let [log-fn (requiring-resolve (symbol "taoensso.timbre" (name level)))]
               (log-fn msg data))))

;;; =============================================================================
;;; Event Handlers
;;; =============================================================================

(ev/reg-event-fx :tmux/pane-created
                 (fn [_cofx [_ {:keys [ling-id pane-id cwd]}]]
                   {:tmux/log {:level :info
                               :msg "Pane created"
                               :data {:ling-id ling-id :pane-id pane-id :cwd cwd}}}))

(ev/reg-event-fx :tmux/pane-killed
                 (fn [_cofx [_ {:keys [ling-id]}]]
                   {:tmux/log {:level :info
                               :msg "Pane killed"
                               :data {:ling-id ling-id}}}))

(ev/reg-event-fx :tmux/task-dispatched
                 (fn [_cofx [_ {:keys [ling-id]}]]
                   {:tmux/log {:level :info
                               :msg "Task dispatched"
                               :data {:ling-id ling-id}}}))

(ev/reg-event-fx :tmux/interrupt-sent
                 (fn [_cofx [_ {:keys [ling-id]}]]
                   {:tmux/log {:level :info
                               :msg "Interrupt sent"
                               :data {:ling-id ling-id}}}))

(ev/reg-event-fx :tmux/state-changed
                 (fn [_cofx [_ {:keys [op ling-id]}]]
                   {:tmux/log {:level :debug
                               :msg "State changed"
                               :data {:op op :ling-id ling-id}}}))

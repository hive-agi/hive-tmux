(ns hive-tmux.state
  "Atom-based pane registry for tmux terminal backend.

   Maps ling-id -> {:pane-id, :window-id, :pane, :window, :cwd}.

   Not DataScript — pane/window are Python object references that can't
   be serialized. The canonical ling registry in DataScript (via ds-lings)
   remains the source of truth for ling lifecycle; this atom is a local
   index for tmux-specific operations."
  (:require [taoensso.timbre :as log]
            [hive.events :as ev]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Event Dispatch (guarded — never breaks state ops)
;;; =============================================================================

(defn- emit!
  "Dispatch a state-changed event. Swallows exceptions to avoid
   breaking registry operations if the event system is not initialized."
  [op ling-id]
  (try
    (ev/dispatch [:tmux/state-changed {:op op :ling-id ling-id}])
    (catch Exception _)))

;;; =============================================================================
;;; Pane Registry
;;; =============================================================================

(defonce ^:private pane-registry
  (atom {}))

(defn register-pane!
  "Register a ling's tmux pane in the local registry.

   Arguments:
     ling-id  - string ling identifier
     pane-info - map from tmux_ops/create-pane!
                 {:pane-id, :window-id, :pane, :window}
     opts      - {:cwd str}

   Returns:
     The registered entry."
  [ling-id pane-info opts]
  (let [entry (merge pane-info (select-keys opts [:cwd]))]
    (swap! pane-registry assoc ling-id entry)
    (log/debug "[tmux-state] Registered pane" {:ling-id ling-id :pane-id (:pane-id entry)})
    (emit! :register ling-id)
    entry))

(defn deregister-pane!
  "Remove a ling's pane from the local registry.
   Returns the removed entry or nil."
  [ling-id]
  (let [entry (get @pane-registry ling-id)]
    (swap! pane-registry dissoc ling-id)
    (when entry
      (log/debug "[tmux-state] Deregistered pane" {:ling-id ling-id})
      (emit! :deregister ling-id))
    entry))

(defn get-pane
  "Look up a ling's pane info. Returns map or nil."
  [ling-id]
  (get @pane-registry ling-id))

(defn get-all-panes
  "Return all registered pane entries as {ling-id -> entry}."
  []
  @pane-registry)

(defn registered?
  "Check if a ling has a registered pane."
  [ling-id]
  (contains? @pane-registry ling-id))

(defn clear-registry!
  "Clear all pane registrations. Used during shutdown."
  []
  (reset! pane-registry {})
  (log/debug "[tmux-state] Registry cleared")
  (emit! :clear nil))

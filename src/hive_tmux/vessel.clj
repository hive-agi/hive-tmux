(ns hive-tmux.vessel
  "TmuxVessel — IVessel implementation for tmux host environment.

   Uses requiring-resolve for all hive-mcp dependencies (zero compile-time coupling).
   Follows EmacsVessel pattern — slave lookup via ISwarmRegistry/get-slave protocol.

   Capabilities: #{:terminal} (no editor, delivery, or repl)."
  (:require [hive-dsl.result :refer [guard rescue]]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; Dependency Resolution (requiring-resolve, no direct coupling)
;;; ============================================================================

(defn- try-resolve
  "Attempt to resolve a fully-qualified symbol. Returns var or nil.
   Uses guard instead of raw try-catch."
  [sym]
  (guard Exception nil (requiring-resolve sym)))

(defn- get-slave-via-protocol
  "Look up a slave through ISwarmRegistry/get-slave (backend-agnostic).
   Returns slave map or nil."
  [agent-id]
  (when-let [get-registry (try-resolve 'hive-mcp.swarm.datascript.registry/get-default-registry)]
    (when-let [get-slave (try-resolve 'hive-mcp.swarm.protocol/get-slave*)]
      (get-slave (get-registry) agent-id))))

(defn- derive-project-id
  "Derive project-id from directory via scope lookup. Returns string or nil."
  [directory]
  (when directory
    (when-let [get-pid (try-resolve 'hive-mcp.tools.memory.scope/get-current-project-id)]
      (get-pid directory))))

(defn- resolve-terminal-addon
  "Get tmux terminal addon via terminal registry."
  []
  (when-let [get-fn (try-resolve 'hive-mcp.agent.ling.terminal-registry/get-terminal-addon)]
    (get-fn :tmux)))

;;; ============================================================================
;;; TmuxVessel
;;; ============================================================================

(defn create-tmux-vessel
  "Create a TmuxVessel implementing IVessel.
   Returns nil if IVessel protocol is not on classpath."
  []
  (when-let [_proto (try-resolve 'hive-mcp.protocols.vessel/IVessel)]
    (reify
      hive-mcp.protocols.vessel/IVessel

      (vessel-id [_] :tmux)

      (capabilities [_] #{:terminal})

      (resolve-context [_ agent-id]
        (when agent-id
          (rescue nil
                  (when-let [slave (get-slave-via-protocol agent-id)]
                    (let [cwd (:slave/cwd slave)
                          project-id (or (:slave/project-id slave)
                                         (derive-project-id cwd))]
                      (when (or cwd project-id)
                        {:cwd cwd
                         :project-id project-id}))))))

      (addon [_ capability]
        (case capability
          :terminal (resolve-terminal-addon)
          nil))

      (initialize! [_ config]
        (log/info "TmuxVessel initialized" (when config {:config-keys (keys config)}))
        nil)

      (shutdown! [_]
        (log/info "TmuxVessel shut down")
        nil))))

;;; ============================================================================
;;; Registration Helpers
;;; ============================================================================

(defn register-tmux-vessel!
  "Create and register the TmuxVessel. Returns vessel or nil."
  []
  (when-let [vessel (create-tmux-vessel)]
    (when-let [register-fn (try-resolve 'hive-mcp.protocols.vessel/register-vessel!)]
      (register-fn vessel)
      (log/info "TmuxVessel registered")
      vessel)))

(defn unregister-tmux-vessel!
  "Unregister the TmuxVessel."
  []
  (when-let [unreg-fn (try-resolve 'hive-mcp.protocols.vessel/unregister-vessel!)]
    (unreg-fn :tmux)
    (log/info "TmuxVessel unregistered")))

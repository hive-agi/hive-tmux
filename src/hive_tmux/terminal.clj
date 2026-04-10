(ns hive-tmux.terminal
  "ITerminalAddon implementation for tmux-based ling lifecycle.

   Spawns Claude CLI instances in tmux panes via libtmux.
   All hive-mcp dependencies resolved at runtime via requiring-resolve
   (zero compile-time coupling).

   Uses hive-dsl rescue/guard instead of raw try-catch."
  (:require [hive-tmux.python.tmux-ops :as tmux-ops]
            [hive-tmux.state :as state]
            [hive-tmux.events :as tmux-ev]
            [hive.events :as ev]
            [clojure.string :as str]
            [hive-dsl.result :refer [guard rescue]]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Runtime Resolution (guard-based try-resolve)
;; =============================================================================

(defn- try-resolve
  "Attempt to resolve a fully-qualified symbol. Returns var or nil.
   Uses guard instead of raw try-catch."
  [sym]
  (guard Exception nil (requiring-resolve sym)))

;; =============================================================================
;; Claude CLI Command Building
;; =============================================================================

(def ^:private default-claude-command "claude")

(defn- build-env-exports
  "Build environment variable export string for the tmux pane shell."
  [{:keys [id depth master]}]
  (let [depth (or depth 1)
        master (or master "direct")]
    (format "export CLAUDE_SWARM_SLAVE_ID=%s CLAUDE_SWARM_DEPTH=%d CLAUDE_SWARM_MASTER=%s"
            (pr-str id) depth (pr-str master))))

(defn- build-claude-command
  "Build the full Claude CLI command string to run in a tmux pane.

   Arguments:
     ctx  - {:id :cwd :presets}
     opts - {:task :system-prompt}

   Returns:
     String command to send to tmux pane."
  [ctx opts]
  (str (build-env-exports {:id (:id ctx)
                           :depth (or (:depth opts) 1)
                           :master (:master opts)})
       " && cd " (pr-str (or (:cwd ctx) "/tmp"))
       " && " default-claude-command
       " --print-session-id"))

;; =============================================================================
;; ITerminalAddon Implementation
;; =============================================================================

(defn make-tmux-terminal
  "Create an ITerminalAddon reify for tmux backend.
   Returns nil if ITerminalAddon protocol is not on classpath."
  []
  (when (try-resolve 'hive-mcp.addons.terminal/ITerminalAddon)
    (tmux-ev/ensure-init!)
    (reify
      hive-mcp.addons.terminal/ITerminalAddon

      (terminal-id [_] :tmux)

      (terminal-spawn! [_ ctx opts]
        (let [{:keys [id cwd]} ctx
              pane-info (tmux-ops/create-pane! {:ling-id id :cwd cwd})
              pane (:pane pane-info)
              cmd (build-claude-command ctx opts)]
          (state/register-pane! id pane-info {:cwd cwd})
          (tmux-ops/send-keys! pane cmd)
          (guard Exception nil
                 (ev/dispatch [:tmux/pane-created {:ling-id id
                                                   :pane-id (:pane-id pane-info)
                                                   :cwd cwd}]))
          id))

      (terminal-dispatch! [_ ctx task-opts]
        (let [{:keys [id]} ctx
              {:keys [task]} task-opts
              pane-entry (state/get-pane id)]
          (if-not pane-entry
            (throw (ex-info "No tmux pane found for ling"
                            {:ling-id id :error "Pane not registered"}))
            (let [pane (:pane pane-entry)
                  escaped-task (-> task
                                   (str/replace "\\" "\\\\")
                                   (str/replace "\"" "\\\""))]
              (tmux-ops/send-keys! pane escaped-task)
              (guard Exception nil
                     (ev/dispatch [:tmux/task-dispatched {:ling-id id}]))
              true))))

      (terminal-status [_ ctx ds-status]
        (let [{:keys [id]} ctx
              pane-entry (state/get-pane id)
              pane-id (:pane-id pane-entry)
              alive? (when pane-id (tmux-ops/pane-alive? pane-id))]
          (if ds-status
            (cond-> ds-status
              (some? alive?) (assoc :tmux-alive? alive?)
              (and pane-entry (not alive?)) (assoc :slave/status :dead))
            (when pane-entry
              {:slave/id id
               :slave/status (if alive? :unknown :dead)
               :tmux-alive? (boolean alive?)
               :tmux-pane-id pane-id}))))

      (terminal-kill! [_ ctx]
        (let [{:keys [id]} ctx
              pane-entry (state/get-pane id)]
          (if-not pane-entry
            (do
              (log/warn "No tmux pane found for ling — already dead?" {:id id})
              {:killed? true :id id :reason :no-pane})
            (let [pane (:pane pane-entry)]
              ;; Graceful: C-c then wait, then kill
              (rescue nil
                      (tmux-ops/send-interrupt! pane)
                      (Thread/sleep 500))
              (tmux-ops/kill-pane! pane)
              (state/deregister-pane! id)
              (guard Exception nil
                     (ev/dispatch [:tmux/pane-killed {:ling-id id}]))
              {:killed? true :id id}))))

      (terminal-interrupt! [_ ctx]
        (let [{:keys [id]} ctx
              pane-entry (state/get-pane id)]
          (if-not pane-entry
            {:success? false
             :ling-id id
             :errors ["No tmux pane found for ling"]}
            (do
              (tmux-ops/send-interrupt! (:pane pane-entry))
              (guard Exception nil
                     (ev/dispatch [:tmux/interrupt-sent {:ling-id id}]))
              {:success? true
               :ling-id id})))))))

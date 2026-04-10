(ns hive-tmux.init
  "IAddon implementation for hive-tmux — tmux terminal backend.

   Follows the vterm-mcp exemplar: reify + nil-railway pipeline.
   Zero compile-time hive-mcp dependencies — all resolved via requiring-resolve.

   Usage:
     ;; Via addon system (auto-discovered from META-INF manifest):
     (init-as-addon!)"
  (:require [hive-tmux.terminal :as terminal]
            [hive-tmux.python.bridge :as py-bridge]
            [hive-tmux.state :as tmux-state]
            [hive-tmux.swarm-bridge :as swarm-bridge]
            [hive-tmux.vessel :as vessel]
            [hive-dsl.result :refer [guard rescue]]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Resolution Helpers
;; =============================================================================

(defn- try-resolve
  "Attempt to resolve a fully-qualified symbol. Returns var or nil.
   Uses guard instead of raw try-catch."
  [sym]
  (guard Exception nil (requiring-resolve sym)))

;; =============================================================================
;; IAddon Implementation
;; =============================================================================

(defonce ^:private addon-instance (atom nil))

(defn- make-addon
  "Create an IAddon reify for hive-tmux.
   Returns nil if protocol is not on classpath."
  []
  (when (try-resolve 'hive-mcp.addons.protocol/IAddon)
    (let [state (atom {:initialized? false})]
      (reify
        hive-mcp.addons.protocol/IAddon

        (addon-id [_] "hive.tmux")

        (addon-type [_] :native)

        (capabilities [_] #{:terminal :health-reporting})

        (initialize! [_ _config]
          (if (:initialized? @state)
            {:success? true :already-initialized? true}
            ;; Step 1: Preflight — Python, libtmux, tmux binary
            (let [preflight (py-bridge/run-preflight)]
              (if (not= :preflight/available (:adt/variant preflight))
                (let [hint (py-bridge/remediation-hint preflight)]
                  (log/error "hive-tmux preflight failed"
                             {:status (:adt/variant preflight) :hint hint})
                  {:success? false
                   :errors [(str "Preflight check failed: " (name (:adt/variant preflight)))
                            (str "Fix: " hint)]})
                ;; Step 2: Initialize Python bridge
                (if-not (py-bridge/ensure-python!)
                  {:success? false
                   :errors ["Python bridge initialization failed after preflight passed (unexpected)"]}
                  ;; Step 3: Verify tmux server reachable
                  (let [server-ok? (guard Exception false
                                          (require 'hive-tmux.python.tmux-ops)
                                          (let [ensure-srv! (requiring-resolve 'hive-tmux.python.tmux-ops/ensure-server!)]
                                            (ensure-srv!)
                                            true))]
                    (if-not server-ok?
                      {:success? false
                       :errors ["tmux server not reachable — is tmux running?"
                                "Fix: tmux new-session -d -s hive"]}
                      ;; Step 4: Create terminal addon
                      (let [tmux-addon (terminal/make-tmux-terminal)]
                        (if-not tmux-addon
                          {:success? false
                           :errors ["ITerminalAddon protocol not on classpath — is hive-mcp loaded?"]}
                          ;; Step 5: Register terminal backend
                          (if-let [register-fn (try-resolve 'hive-mcp.agent.ling.terminal-registry/register-terminal!)]
                            (let [result (register-fn :tmux tmux-addon)]
                              (if (:registered? result)
                                (do
                                  ;; Step 5.5: Register :tmux as spawn mode
                                  (when-let [reg-mode! (try-resolve 'hive-mcp.agent.spawn-mode-registry/register-mode!)]
                                    (reg-mode! :tmux
                                               {:description     "tmux pane — libtmux via libpython-clj"
                                                :requires-emacs? false
                                                :io-model        :buffer
                                                :slot-limit      6
                                                :mcp?             true
                                                :alias-of         nil
                                                :capabilities     #{:interactive :dispatch :kill :interrupt}}))
                                  ;; Step 6: Register vessel
                                  (vessel/register-tmux-vessel!)
                                  ;; Step 7: Install swarm event bridge so tmux pane
                                  ;; lifecycle events reach hive-mcp.channel.core +
                                  ;; NATS backbone (and thus swarm/sync handlers).
                                  (guard Exception nil (swarm-bridge/start!))
                                  (reset! state {:initialized? true
                                                 :terminal-addon tmux-addon})
                                  (log/info "hive-tmux addon initialized — :tmux terminal + spawn-mode registered")
                                  {:success? true
                                   :errors []
                                   :metadata {:terminal-id :tmux}})
                                (do
                                  (log/error "Failed to register tmux terminal" {:result result})
                                  {:success? false
                                   :errors (or (:errors result) ["Registration failed"])})))
                            (do
                              (log/error "terminal-registry/register-terminal! not available on classpath")
                              {:success? false
                               :errors ["Terminal registry not available — is hive-mcp loaded?"]})))))))))))

        (shutdown! [_]
          (when (:initialized? @state)
            ;; Uninstall swarm event bridge (restores log-only handlers)
            (guard Exception nil (swarm-bridge/stop!))
            ;; Deregister terminal backend
            (when-let [dereg-fn (try-resolve 'hive-mcp.agent.ling.terminal-registry/deregister-terminal!)]
              (dereg-fn :tmux))
            ;; Deregister spawn mode
            (when-let [dereg-mode! (try-resolve 'hive-mcp.agent.spawn-mode-registry/deregister-mode!)]
              (dereg-mode! :tmux))
            ;; Unregister vessel
            (vessel/unregister-tmux-vessel!)
            ;; Clear pane state
            (tmux-state/clear-registry!)
            (reset! state {:initialized? false})
            (log/info "hive-tmux addon shut down"))
          nil)

        (tools [_] [])

        (schema-extensions [_] {})

        (health [_]
          (if (:initialized? @state)
            {:status :ok
             :details {:terminal-id :tmux
                       :pane-count (count (tmux-state/get-all-panes))}}
            (let [preflight (py-bridge/run-preflight)]
              {:status :down
               :details {:reason "not initialized"
                         :preflight (:adt/variant preflight)
                         :hint (when (not= :preflight/available (:adt/variant preflight))
                                 (py-bridge/remediation-hint preflight))}})))))))

;; =============================================================================
;; Dep Registry + Nil-Railway Pipeline
;; =============================================================================

(defonce ^:private dep-registry
  (atom {:register! 'hive-mcp.addons.core/register-addon!
         :init!     'hive-mcp.addons.core/init-addon!
         :addon-id  'hive-mcp.addons.protocol/addon-id}))

(defn- resolve-deps
  "Resolve all symbols in registry. Returns ctx map or nil."
  [registry]
  (reduce-kv
   (fn [ctx k sym]
     (if-let [resolved (try-resolve sym)]
       (assoc ctx k resolved)
       (do (log/debug "Dep resolution failed:" k "->" sym)
           (reduced nil))))
   {}
   registry))

(defn- step-resolve-deps [ctx]
  (when-let [deps (resolve-deps @dep-registry)]
    (merge ctx deps)))

(defn- step-register [{:keys [addon register!] :as ctx}]
  (let [result (register! addon)]
    (when (:success? result)
      (assoc ctx :reg-result result))))

(defn- step-init [{:keys [addon addon-id init!] :as ctx}]
  (let [result (init! (addon-id addon))]
    (when (:success? result)
      (assoc ctx :init-result result))))

(defn- step-store-instance [{:keys [addon] :as ctx}]
  (reset! addon-instance addon)
  ctx)

(defn- run-addon-pipeline!
  "Nil-railway: resolve-deps -> register -> init -> store"
  [initial-ctx]
  (some-> initial-ctx
          step-resolve-deps
          step-register
          step-init
          step-store-instance))

;; =============================================================================
;; Public API
;; =============================================================================

(defn init-as-addon!
  "Register hive-tmux as an IAddon. Returns registration result."
  []
  (if-let [_result (some-> (make-addon)
                           (as-> addon (run-addon-pipeline! {:addon addon})))]
    (do
      (log/info "hive-tmux registered as IAddon")
      {:registered ["tmux"] :total 1})
    (do
      (log/debug "IAddon unavailable — hive-tmux addon registration failed")
      {:registered [] :total 0})))

(defn get-addon-instance
  "Return the current IAddon instance, or nil."
  []
  @addon-instance)

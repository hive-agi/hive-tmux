(ns hive-tmux.swarm-bridge
  "Bridge tmux pane lifecycle events into hive-mcp's swarm event backbone.

   Problem:
   - hive-tmux emits re-frame-style events (`:tmux/pane-created`,
     `:tmux/pane-killed`, …) via `hive.events`. These are local to the
     hive-events router and never reach `hive-mcp.channel.core`, NATS,
     or the swarm/sync handlers.
   - When a tmux pane is created *outside* the standard `Ling/spawn!`
     flow (or when the JVM spawn path is bypassed for any reason), the
     swarm registry never learns about the slave and `swarm_status`
     shows nothing.

   Solution:
   - On addon initialization, re-register the `:tmux/pane-created` and
     `:tmux/pane-killed` event handlers with wrapping handlers that
     preserve the original `:tmux/log` effect and ALSO emit a new
     `:swarm/publish-slave-event` effect.
   - The `:swarm/publish-slave-event` effect is handled by a fx
     registered here; it uses `requiring-resolve` to locate
     `hive-mcp.swarm.event-bridge/publish-slave-event!` and forwards
     the translated payload.
   - On shutdown, the original handlers are reinstated so that
     subsequent dispatches fall back to pure logging.

   Design (SOLID/DDD/FP):
   - SRP: this ns ONLY translates `:tmux/pane-*` → `:slave-*` and
     forwards via `publish-slave-event!`. No direct DataScript writes,
     no NATS coupling.
   - DIP: depends on `hive.events` (stable re-frame API) and on a
     `requiring-resolve`d var from hive-mcp. No compile-time coupling
     to hive-mcp.
   - OCP: adding a new tmux event → slave event mapping is a single
     entry in `event->slave-type` plus a pure payload-translator fn.
   - Pure helpers (`translate-pane-created`, `translate-pane-killed`)
     are side-effect free and trivially testable. Side effects live
     at the boundary (`publish!`, `start!`, `stop!`).

   Loop safety:
   - `publish-slave-event!` already handles NATS loop prevention via
     the `:via :nats-bridge` tag. Events emitted from this bridge are
     additionally tagged with `:source :hive-tmux` for traceability.
   - Duplication with the JVM `Ling/spawn!` path is harmless because
     `add-slave!` in ds-lings is idempotent (upsert-style) and the
     swarm/sync handlers reconcile by slave-id."
  (:require [hive.events :as ev]
            [hive-tmux.state :as tmux-state]
            [hive-dsl.result :refer [guard rescue]]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Runtime resolution (no compile-time coupling to hive-mcp)
;; =============================================================================

(defn- try-resolve
  "Attempt to resolve a fully-qualified symbol. Returns var or nil."
  [sym]
  (guard Exception nil (requiring-resolve sym)))

(defn- resolve-publish-slave-event!
  "Resolve `hive-mcp.swarm.event-bridge/publish-slave-event!` at call time.
   Returns the var or nil when hive-mcp is not on the classpath."
  []
  (try-resolve 'hive-mcp.swarm.event-bridge/publish-slave-event!))

;; =============================================================================
;; Pure translators: tmux pane payload → slave lifecycle event payload
;; =============================================================================

(defn- derive-project-id
  "Derive a project-id from a cwd via hive-mcp scope lookup.
   Pure with respect to the JVM classpath: returns nil if the scope ns
   is absent. Called only from the pure translators below."
  [cwd]
  (when cwd
    (when-let [get-pid (try-resolve 'hive-mcp.tools.memory.scope/get-current-project-id)]
      (rescue nil (get-pid cwd)))))

(defn- pane-entry-cwd
  "Look up cwd from the tmux pane registry. Falls back gracefully."
  [ling-id]
  (rescue nil (:cwd (tmux-state/get-pane ling-id))))

(defn translate-pane-created
  "Pure(ish) translation of a `:tmux/pane-created` payload into a
   `:slave-spawned` event map consumed by
   `hive-mcp.swarm.event-bridge/publish-slave-event!`.

   Input payload shape (from `hive-tmux.events`):
     {:ling-id str :pane-id str :cwd str}

   Tmux panes always represent depth-1 lings (ling = one pane = one
   Claude CLI process)."
  [{:keys [ling-id cwd] :as _payload}]
  (let [cwd (or cwd (pane-entry-cwd ling-id))]
    {:type       :slave-spawned
     :slave-id   ling-id
     :name       ling-id
     :depth      1
     :cwd        cwd
     :project-id (derive-project-id cwd)
     :source     :hive-tmux
     :timestamp  (System/currentTimeMillis)}))

(defn translate-pane-killed
  "Pure translation of a `:tmux/pane-killed` payload into a
   `:slave-killed` event map."
  [{:keys [ling-id] :as _payload}]
  {:type      :slave-killed
   :slave-id  ling-id
   :source    :hive-tmux
   :timestamp (System/currentTimeMillis)})

(def ^:private event->translator
  "Registry of `:tmux/*` event-id → pure translator fn. OCP extension point."
  {:tmux/pane-created translate-pane-created
   :tmux/pane-killed  translate-pane-killed})

;; =============================================================================
;; Effect handler: :swarm/publish-slave-event
;; =============================================================================

(defn- publish-slave-event-fx
  "Side-effecting fx handler. Forwards a translated payload to
   `hive-mcp.swarm.event-bridge/publish-slave-event!`. Swallows errors
   so the hive-events router never sees a failure from the bridge."
  [event-map]
  (rescue nil
    (if-let [publish! (resolve-publish-slave-event!)]
      (do (publish! event-map)
          (log/debug "[hive-tmux.swarm-bridge] published" (:type event-map)
                     {:slave-id (:slave-id event-map)}))
      (log/debug "[hive-tmux.swarm-bridge] publish-slave-event! unavailable — skipping"
                 {:type (:type event-map)}))))

;; =============================================================================
;; Wrapped event handlers (preserve the original :tmux/log effect)
;; =============================================================================

(defn- wrapped-pane-created
  "Replacement handler for `:tmux/pane-created`. Preserves the original
   `:tmux/log` effect and additionally emits a `:swarm/publish-slave-event`
   effect carrying the translated `:slave-spawned` payload."
  [_cofx [_ {:keys [ling-id pane-id cwd] :as payload}]]
  {:tmux/log {:level :info
              :msg   "Pane created"
              :data  {:ling-id ling-id :pane-id pane-id :cwd cwd}}
   :swarm/publish-slave-event (translate-pane-created payload)})

(defn- wrapped-pane-killed
  "Replacement handler for `:tmux/pane-killed`. Preserves the original
   `:tmux/log` effect and additionally emits a `:swarm/publish-slave-event`
   effect carrying the translated `:slave-killed` payload."
  [_cofx [_ {:keys [ling-id] :as payload}]]
  {:tmux/log {:level :info
              :msg   "Pane killed"
              :data  {:ling-id ling-id}}
   :swarm/publish-slave-event (translate-pane-killed payload)})

(defn- original-pane-created
  "The original (logging-only) handler, reinstated on shutdown."
  [_cofx [_ {:keys [ling-id pane-id cwd]}]]
  {:tmux/log {:level :info
              :msg   "Pane created"
              :data  {:ling-id ling-id :pane-id pane-id :cwd cwd}}})

(defn- original-pane-killed
  [_cofx [_ {:keys [ling-id]}]]
  {:tmux/log {:level :info
              :msg   "Pane killed"
              :data  {:ling-id ling-id}}})

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defonce ^:private bridge-state (atom {:running false}))

(defn start!
  "Install the swarm bridge.

   - Registers the `:swarm/publish-slave-event` effect handler.
   - Re-registers wrapping handlers for `:tmux/pane-created` and
     `:tmux/pane-killed` that keep the original logging behavior and
     additionally forward translated events to
     `hive-mcp.swarm.event-bridge/publish-slave-event!`.

   Idempotent: a second call is a no-op.
   Returns true on (new) start, false if already running."
  []
  (if (:running @bridge-state)
    (do (log/debug "[hive-tmux.swarm-bridge] already running") false)
    (do
      (ev/reg-fx :swarm/publish-slave-event publish-slave-event-fx)
      (ev/reg-event-fx :tmux/pane-created wrapped-pane-created)
      (ev/reg-event-fx :tmux/pane-killed  wrapped-pane-killed)
      (swap! bridge-state assoc
             :running true
             :translators (set (keys event->translator)))
      (log/info "[hive-tmux.swarm-bridge] installed — tmux pane lifecycle → swarm event bridge active"
                {:events (keys event->translator)})
      true)))

(defn stop!
  "Uninstall the swarm bridge.

   Reinstates the pure logging handlers for `:tmux/pane-created` and
   `:tmux/pane-killed`. The `:swarm/publish-slave-event` effect handler
   remains registered but is harmless — no handler will emit it.

   Idempotent."
  []
  (when (:running @bridge-state)
    (ev/reg-event-fx :tmux/pane-created original-pane-created)
    (ev/reg-event-fx :tmux/pane-killed  original-pane-killed)
    (swap! bridge-state assoc :running false)
    (log/info "[hive-tmux.swarm-bridge] uninstalled — pane events now log-only"))
  nil)

(defn status
  "Return current bridge state for diagnostics."
  []
  @bridge-state)

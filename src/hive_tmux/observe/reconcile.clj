(ns hive-tmux.observe.reconcile
  "Turns one hook arrival into a registry action.

   The registry is keyed by ling-id while an arrival carries only a pane-id, so
   reconciling needs a reverse lookup. Planning is pure over a registry VALUE:
   the decision is testable without a registry, a tmux server or a socket, and
   the boundary that performs it stays a `case` over the plan."
  (:require [hive-tmux.observe.schema :as schema]
            [hive-tmux.observe.wire :as wire]
            [malli.core :as m]))

(defn ling-for-pane
  "The ling-id whose registry entry holds PANE-ID, or nil.

   REGISTRY is the `{ling-id -> entry}` value. Ambiguity is resolved by taking
   the first match in sort order rather than an arbitrary one, so the same
   registry always yields the same answer; two lings sharing a pane is a bug
   upstream and must not also be non-deterministic here."
  [registry pane-id]
  (when pane-id
    (->> registry
         (filter (fn [[_ entry]] (= pane-id (:pane-id entry))))
         (sort-by key)
         ffirst)))

(defn plan
  "The registry action EVENT calls for, given REGISTRY.

   Only a pane-death id deregisters. A `:tmux/state-changed` arrival carries no
   claim that a pane is gone, and acting on it would evict live panes; it is
   observed and ignored."
  [registry event]
  (let [{:keys [event pane]} event]
    (cond
      (= wire/unknown-event-id event)
      {:action :ignore :reason :unknown-hook}

      (not= :tmux/pane-killed event)
      {:action :ignore :reason :not-a-death}

      :else
      (if-let [ling-id (ling-for-pane registry pane)]
        {:action :deregister :ling-id ling-id :pane pane}
        {:action :ignore :reason :pane-not-registered :pane pane}))))

(m/=> ling-for-pane
      [:=> [:cat [:map-of :string [:map [:pane-id {:optional true} [:maybe :string]]]]
            [:maybe schema/PaneId]]
       [:maybe :string]])

(m/=> plan
      [:=> [:cat [:map-of :string [:map [:pane-id {:optional true} [:maybe :string]]]]
            schema/HookEvent]
       [:map [:action :keyword]]])

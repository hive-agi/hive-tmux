(ns hive-tmux.observe.ingest
  "Joins a hook arrival to the registry and the event system.

   Effects arrive as an injected map (`:registry` `:deregister!` `:dispatch!`)
   rather than as direct calls on `hive-tmux.state` and `hive-events`, so the
   whole arrival path is exercised against recording fns."
  (:require [hive-tmux.observe.reconcile :as reconcile]
            [taoensso.timbre :as log]
            [hive-tmux.state :as state]
            [hive.events :as ev]))

(defn default-ports
  "The production effects: the live pane registry and the event system."
  []
  {:registry    state/get-all-panes
   :deregister! state/deregister-pane!
   :dispatch!   ev/dispatch})

(defn handle
  "Apply one hook EVENT through PORTS. Returns the plan that was carried out.

   The registry action runs before the event dispatch so a handler reading the
   registry sees it already reconciled: an externally-killed pane must not
   still be registered while its own death event is being handled."
  [{:keys [registry deregister! dispatch!]} event]
  (let [p (reconcile/plan (registry) event)]
    (when (= :deregister (:action p))
      (deregister! (:ling-id p))
      (log/info "hive-tmux observe: deregistered externally-killed pane"
                {:ling-id (:ling-id p) :pane (:pane p)}))
    (dispatch! [(:event event) {:pane-id (:pane event)
                                :ling-id (:ling-id p)
                                :hook (:hook event)
                                :source :tmux-hook}])
    p))

(defn dispatch-fn
  "The listener `dispatch!` for PORTS.

   Never throws: the listener's accept loop must survive a handler fault, and
   an arrival that cannot be reconciled is a logged event rather than a dead
   ingestion path."
  [ports]
  (fn [event]
    (try
      (handle ports event)
      (catch Exception e
        (log/warn e "hive-tmux observe: arrival handling failed" {:event event})
        {:action :ignore :reason :handler-failed}))))

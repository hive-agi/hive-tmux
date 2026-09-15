(ns hive-tmux.observe.schema
  "Malli value objects for tmux-originated event ingestion.

   JVM-only on purpose: malli arrives as a `:mvn/version` coordinate, which
   ClojureWasm does not resolve. The portable half of the subsystem lives in
   `hive-tmux.observe.wire` and carries no contracts of its own; these schemas
   describe its values from the JVM side, where they can be enforced and can
   generate tests.")

(def HookName
  "A tmux hook name, as it appears in `set-hook`."
  [:string {:min 1}])

(def PaneId
  "A tmux pane id (`%14`). Modelled permissively: the hook client forwards
   whatever tmux substituted, and an unresolved format is a string too."
  [:string {:min 1}])

(def EventId
  "The hive-events id an arrival dispatches."
  :keyword)

(def HookEvent
  "One hook firing as it crosses the socket.

   Open rather than `:closed true`: the client is a separately deployed binary
   and may be a version ahead of the listener, so an unknown extra key must
   travel rather than fail validation."
  [:map
   [:hook  HookName]
   [:event EventId]
   [:pane  [:maybe PaneId]]
   [:tmux  [:maybe :string]]
   [:at    :int]])

(def SocketPath
  "Filesystem path of the listener's UNIX socket.

   Capped at 100 to stay under the OS `sun_path` limit of 108 bytes, which is
   the kernel's cap and not the filesystem's: an over-long path fails at bind
   with an opaque error, so it is rejected here instead."
  [:string {:min 1 :max 100}])

(def HookCommand
  "One tmux invocation, as an argv vector."
  [:vector {:min 1} :string])

(def ObserveConfig
  "Operator configuration for the ingestion subsystem."
  [:map {:closed true}
   [:socket-path SocketPath]
   [:client-cmd [:vector {:min 1} :string]]
   [:hooks [:vector {:min 1} HookName]]])

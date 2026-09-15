(ns hive-tmux.observe.wire
  "Wire format shared by the JVM listener and the cljw hook client.

   This namespace is loaded by ClojureWasm, which resolves only `:git/url` and
   `:local/root` coordinates. It therefore requires nothing outside
   `clojure.core` and `clojure.edn`, and must stay that way: a `:mvn/version`
   dependency reached from here is silently skipped by cljw and fails at call
   time rather than load time."
  (:require [clojure.edn :as edn]))

(def hook->event-id
  "tmux hook name -> the hive-events id an arrival dispatches.

   The map is the open point: a newly observed hook is a new entry, not an edit
   to `classify`. Hooks absent here are classified `:tmux/unknown-hook` rather
   than guessed into an existing id."
  {"pane-died"           :tmux/pane-killed
   "pane-exited"         :tmux/pane-killed
   "window-pane-changed" :tmux/state-changed
   "window-linked"       :tmux/state-changed
   "window-unlinked"     :tmux/state-changed
   "session-closed"      :tmux/state-changed
   "client-attached"     :tmux/state-changed
   "client-detached"     :tmux/state-changed
   "alert-activity"      :tmux/state-changed})

(def unknown-event-id
  "Classification of a hook name the table does not carry."
  :tmux/unknown-hook)

(defn classify
  "The hive-events id for tmux hook name HOOK, or `unknown-event-id`."
  [hook]
  (get hook->event-id hook unknown-event-id))

(defn args->event
  "The event map for one hook firing.

   ARGS is the hook client's `*command-line-args*` tail: hook name, then the
   tmux pane id. NOW-MS and TMUX-ENV are supplied by the caller so this stays
   pure and testable off the clock."
  [args now-ms tmux-env]
  (let [[hook pane] args]
    {:hook    hook
     :event   (classify hook)
     :pane    pane
     :tmux    tmux-env
     :at      now-ms}))

(defn encode
  "EVENT as the wire string."
  [event]
  (pr-str event))

(defn decode
  "The event carried by wire string S, or nil when S is not readable EDN.

   `edn/read-string` never evaluates, so a malformed or hostile payload from
   the socket is a nil here rather than a code path."
  [s]
  (try
    (let [v (edn/read-string s)]
      (when (map? v) v))
    (catch Exception _ nil)))

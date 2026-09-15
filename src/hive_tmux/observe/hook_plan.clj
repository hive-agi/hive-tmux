(ns hive-tmux.observe.hook-plan
  "Pure planning of the tmux `set-hook` invocations that install and remove
   ingestion.

   The plan is DATA: a vector of argv vectors. Nothing here runs tmux, so the
   whole installation surface is testable without a server, and the boundary
   that does run it stays a one-liner."
  (:require [hive-tmux.observe.schema :as schema]
            [malli.core :as m]
            [clojure.string :as str]))

(def default-hooks
  "The hooks worth observing: the ones tmux fires for events hive did not
   cause. Hive already emits its own events for panes it creates and kills, so
   observing those would double-count; these are the arrivals that are
   otherwise invisible."
  ["pane-died"
   "pane-exited"
   "window-linked"
   "window-unlinked"
   "session-closed"
   "client-attached"
   "client-detached"])

(defn hook-payload
  "The shell command tmux runs when HOOK fires.

   `#{hook_pane}` is substituted by tmux with the pane the hook fired for. The
   socket path precedes the hook name so the client's argv stays positional and
   free of flag parsing."
  [{:keys [socket-path client-cmd]} hook]
  (str (str/join " " client-cmd)
       " " socket-path
       " " hook
       " '#{hook_pane}'"))

(defn install-plan
  "The argv vectors that install ingestion for CONFIG.

   `-g` sets the hook globally so it survives session creation; without it a
   hook reaches only the session that happened to exist at install time."
  [{:keys [hooks] :as config}]
  (mapv (fn [hook]
          ["set-hook" "-g" hook (str "run-shell " (pr-str (hook-payload config hook)))])
        hooks))

(defn uninstall-plan
  "The argv vectors that remove ingestion for CONFIG.

   `-u` unsets rather than setting empty: an empty hook is still a hook and
   tmux would run it."
  [{:keys [hooks]}]
  (mapv (fn [hook] ["set-hook" "-gu" hook]) hooks))

(m/=> hook-payload [:=> [:cat schema/ObserveConfig schema/HookName] :string])
(m/=> install-plan [:=> [:cat schema/ObserveConfig] [:vector schema/HookCommand]])
(m/=> uninstall-plan [:=> [:cat schema/ObserveConfig] [:vector schema/HookCommand]])

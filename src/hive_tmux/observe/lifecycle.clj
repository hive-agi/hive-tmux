(ns hive-tmux.observe.lifecycle
  "Start and stop tmux-originated ingestion for the addon.

   Installing hooks is OPT-IN and off by default. `set-hook -g` is a persistent,
   server-wide change to the operator's tmux, and an addon that rewrites it on
   every init would be editing an environment it does not own. The listener
   itself is inert (a socket nobody writes to), so it starts by default and the
   operator turns on the half that touches their server."
  (:require [clojure.string :as str]
            [hive-tmux.observe.hook-plan :as plan]
            [hive-tmux.observe.ingest :as ingest]
            [hive-tmux.observe.listener :as listener]
            [taoensso.timbre :as log]))

(defn default-client-cmd
  "The command tmux runs per hook firing.

   Prefers the compiled client named by `$HIVE_TMUX_HOOK_BIN`, else the one
   installed at `~/.local/bin/hive-tmux-hook`, and falls back to running the
   source through cljw.

   Compiling buys no speed (both start in ~17ms), so the reason to prefer the
   binary is that it is self-contained: tmux runs a hook with a cwd the addon
   does not own, and the interpreted form needs a relative classpath, the
   source tree, and prints a deps.edn note on every firing."
  []
  (let [candidates (remove nil?
                           [(System/getenv "HIVE_TMUX_HOOK_BIN")
                            (str (System/getProperty "user.home")
                                 "/.local/bin/hive-tmux-hook")])]
    (or (some (fn [p]
                (when (.canExecute (java.io.File. ^String p)) [p]))
              candidates)
        ["cljw" "-cp" "src:native" "-m" "hive-tmux.observe.hook-client"])))

(defn default-socket-path
  "Where the listener binds when the operator names no path.

   Prefers `$XDG_RUNTIME_DIR`, which is per-user and cleared on logout, so a
   stale socket cannot outlive the session that made it. The path must stay
   under the OS 108-byte `sun_path` cap, which a runtime dir comfortably does."
  []
  (str (or (System/getenv "XDG_RUNTIME_DIR") "/tmp") "/hive-tmux-observe.sock"))

(defn config->observe
  "The ingestion config carried by addon CONFIG, with defaults applied."
  [config]
  {:socket-path (or (:tmux/observe-socket config) (default-socket-path))
   :client-cmd  (or (:tmux/observe-client-cmd config) (default-client-cmd))
   :hooks       (vec (or (:tmux/observe-hooks config) plan/default-hooks))})

(defn enabled?
  "Whether ingestion runs at all. Default true: the listener is inert."
  [config]
  (not (false? (:tmux/observe? config))))

(defn install-hooks?
  "Whether to write `set-hook -g` into the operator's tmux server.

   Default FALSE. This is the only part of ingestion that modifies something
   outside hive, so it is never on by accident."
  [config]
  (true? (:tmux/observe-install-hooks? config)))

(defn- run-tmux!
  "Run one tmux invocation. Returns the exit code."
  [tmux-binary argv]
  (let [pb (ProcessBuilder. ^java.util.List (into [tmux-binary] argv))]
    (.waitFor (.start (doto pb (.redirectErrorStream true))))))

(defn start!
  "Start ingestion for addon CONFIG. Returns a handle for `stop!`, or nil when
   disabled. Never throws: ingestion is an enhancement, and a failure to start
   it must not fail addon initialization."
  ([config] (start! config (partial run-tmux! (or (:tmux/binary config) "tmux"))))
  ([config run!]
   (when (enabled? config)
     (try
       (let [observe (config->observe config)
             l (listener/start! (:socket-path observe)
                                (ingest/dispatch-fn (ingest/default-ports)))]
         (when (install-hooks? config)
           (doseq [argv (plan/install-plan observe)] (run! argv))
           (log/info "hive-tmux observe: installed tmux hooks"
                     {:hooks (str/join "," (:hooks observe))}))
         {:listener l :observe observe :hooks-installed? (install-hooks? config)})
       (catch Exception e
         (log/warn e "hive-tmux observe: ingestion did not start")
         nil)))))

(defn stop!
  "Stop ingestion, removing any hooks this addon installed.

   Hooks are removed only when `start!` installed them: unsetting a hook the
   operator wrote by hand would be destroying their configuration."
  ([handle] (stop! handle (partial run-tmux! "tmux")))
  ([{:keys [listener observe hooks-installed?]} run!]
   (when hooks-installed?
     (doseq [argv (plan/uninstall-plan observe)]
       (try (run! argv) (catch Exception _ nil))))
   (when listener (listener/stop! listener))
   :stopped))

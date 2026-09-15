;;; hive-tmux tmux hook client, run by ClojureWasm.
;;;
;;; tmux invokes this per hook firing, so its whole budget is process startup:
;;; cljw pays ~0.02s and ~11MB, where a JVM pays seconds and would make the
;;; hook itself the bottleneck.
;;;
;;; It lives outside `src` deliberately. cljw's reader feature set impersonates
;;; `:clj`, so a reader conditional cannot separate it from the JVM, and a
;;; `cljw.net` require would break any JVM load of the same file. Keeping it
;;; off the JVM source path is the separation.
;;;
;;;   cljw -cp src -M native/hive_tmux_hook.cljc <socket-path> <hook> <pane>

(require '[cljw.net :as net]
         '[hive-tmux.observe.wire :as wire])

(let [[socket-path & event-args] *command-line-args*
      event (wire/args->event event-args
                              (System/currentTimeMillis)
                              (System/getenv "TMUX"))]
  (if (nil? socket-path)
    (do (binding [*out* *err*]
          (println "usage: hive_tmux_hook.cljc <socket-path> <hook> <pane>"))
        (System/exit 2))
    ;; A hook must never wedge tmux: an unreachable listener is reported and
    ;; exited, not retried. tmux runs this synchronously per event.
    (try
      (let [sock (net/connect-unix socket-path)]
        (.write sock (.getBytes (wire/encode event)))
        (.close sock))
      (catch Exception e
        (binding [*out* *err*]
          (println "hive-tmux hook: listener unreachable:" (.getMessage e)))
        (System/exit 1)))))

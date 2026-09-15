(ns hive-tmux.observe.hook-client
  "tmux hook client: encodes one hook firing and writes it to the listener's
   UNIX socket.

   Contract: `(-main socket-path hook pane)`. Exit 0 on delivery, 2 on a
   missing socket path, 1 when the listener is unreachable.

   This namespace lives under the `native` classpath root, never `src`: it
   requires `cljw.net`, which only ClojureWasm provides, and cljw's reader
   impersonates `:clj`, so a reader conditional cannot keep it off the JVM.
   The directory boundary is the separation.

   Run interpreted:
     cljw -cp src:native -m hive-tmux.observe.hook-client <sock> <hook> <pane>
   Compiled:
     cljw build -m hive-tmux.observe.hook-client -o <bin> -cp src:native"
  (:require [cljw.net :as net]
            [hive-tmux.observe.wire :as wire]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn -main
  "Deliver one hook event to the listener at SOCKET-PATH.

   Never retries: tmux runs a hook synchronously, so an unreachable listener
   is reported and exited rather than waited on."
  [& [socket-path & event-args]]
  (if (nil? socket-path)
    (do (binding [*out* *err*]
          (println "usage: hive-tmux-hook <socket-path> <hook> <pane>"))
        (System/exit 2))
    (let [event (wire/args->event event-args
                                  (System/currentTimeMillis)
                                  (System/getenv "TMUX"))]
      (try
        (let [sock (net/connect-unix socket-path)]
          (.write sock (.getBytes (wire/encode event)))
          (.close sock))
        (catch Exception e
          (binding [*out* *err*]
            (println "hive-tmux hook: listener unreachable:" (.getMessage e)))
          (System/exit 1))))))

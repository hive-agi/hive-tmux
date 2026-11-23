(ns hive-tmux.observe.hook-client
  "tmux hook client: encodes one hook firing and writes it to the listener's
   UNIX socket.

   Contract: `(-main socket-path hook pane)`. SOCKET-PATH `-` means the
   STD-04 T2 default (see `resolve-socket-path`; it assumes /tmp when
   XDG_RUNTIME_DIR is unset). Exit 0 on delivery, 2 on a
   missing or unresolvable socket path, 1 when the listener is unreachable.

   This namespace lives under the `native` classpath root, never `src`: it
   requires `cljw.net`, which only ClojureWasm provides, and cljw's reader
   impersonates `:clj`, so a reader conditional cannot keep it off the JVM.
   The directory boundary is the separation.

   Run interpreted:
     cljw -cp src:native -m hive-tmux.observe.hook-client <sock> <hook> <pane>
   Compiled:
     cljw build -m hive-tmux.observe.hook-client -o <bin> -cp src:native"
  (:require [cljw.net :as net]
            [hive-tmux.observe.socket-path :as socket-path]
            [hive-tmux.observe.wire :as wire]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- current-uid
  "This process's uid from /proc/self/status (ClojureWasm has no getuid),
   or nil where that file cannot be read."
  []
  (try (socket-path/proc-status-uid (slurp "/proc/self/status"))
       (catch Exception _ nil)))

(defn resolve-socket-path
  "The socket to write to. An explicit path is used as given; `-` means the
   STD-04 T2 default, computed with the same pure rule the JVM listener's
   default comes from (hive-system.network.path, copied in
   hive-tmux.observe.socket-path): $XDG_RUNTIME_DIR/hive/tmux-observe.sock,
   else /tmp/hive-<uid>/tmux-observe.sock.

   With XDG_RUNTIME_DIR unset, `-` ASSUMES /tmp: ClojureWasm's
   java.io.tmpdir is always /tmp, whereas a JVM listener started with
   -Djava.io.tmpdir=<dir> binds under <dir>, and `-` then names a different
   socket. Hooks installed by hive-tmux.observe.lifecycle never use `-`:
   they pass the listener's resolved path explicitly. Where /proc is absent
   (macOS) the uid is unknown, so `-` without XDG_RUNTIME_DIR exits 2.
   => {:ok path} or {:error ...}"
  [arg]
  (if (= "-" arg)
    (let [res (socket-path/socket-path {"XDG_RUNTIME_DIR" (System/getenv "XDG_RUNTIME_DIR")}
                                       (System/getProperty "java.io.tmpdir")
                                       (current-uid)
                                       socket-path/service)]
      (if (contains? res :ok) {:ok (:path (:ok res))} res))
    {:ok arg}))

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
                                  (System/getenv "TMUX"))
          path  (resolve-socket-path socket-path)]
      (when-not (contains? path :ok)
        (binding [*out* *err*]
          (println "hive-tmux hook: no socket path:" (:message path)))
        (System/exit 2))
      (try
        (let [sock (net/connect-unix (:ok path))]
          (.write sock (.getBytes (wire/encode event)))
          (.close sock))
        (catch Exception e
          (binding [*out* *err*]
            (println "hive-tmux hook: listener unreachable:" (.getMessage e)))
          (System/exit 1))))))

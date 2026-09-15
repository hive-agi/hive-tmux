(ns hive-tmux.observe.listener
  "UNIX-socket boundary: accepts hook arrivals and hands each decoded event to
   an injected dispatch fn.

   The dispatch fn is an argument rather than a direct `hive-events` call, so
   the accept loop is exercised in tests against a recording fn and carries no
   dependency on the event system it feeds."
  (:require [hive-tmux.observe.wire :as wire]
            [taoensso.timbre :as log])
  (:import [java.io File]
           [java.net StandardProtocolFamily UnixDomainSocketAddress]
           [java.nio ByteBuffer]
           [java.nio.channels ServerSocketChannel]))

(def ^:private read-buffer-bytes
  "Cap on one arrival. A hook event is a small flat map; anything larger is a
   malformed sender rather than a big event, and is truncated into an
   unreadable payload that `wire/decode` rejects."
  8192)

(defn- read-arrival
  "The wire string carried by one accepted channel."
  [ch]
  (let [buf (ByteBuffer/allocate read-buffer-bytes)
        n (.read ch buf)]
    (when (pos? n)
      (String. (.array buf) 0 n "UTF-8"))))

(defn- serve-one!
  "Accept one connection, decode it, and dispatch it. Never throws: a bad
   arrival must not end the accept loop that serves every later one."
  [srv dispatch!]
  (try
    (with-open [ch (.accept srv)]
      (if-let [event (some-> (read-arrival ch) wire/decode)]
        (dispatch! event)
        (log/warn "hive-tmux observe: undecodable hook arrival")))
    (catch java.nio.channels.AsynchronousCloseException _ :closed)
    (catch java.nio.channels.ClosedChannelException _ :closed)
    (catch Exception e
      (log/warn e "hive-tmux observe: hook arrival failed"))))

(defn start!
  "Bind SOCKET-PATH and serve arrivals to DISPATCH! until stopped.

   Returns a map carrying the server and the accept thread; pass it to
   `stop!`. A stale socket file is removed first: `bind` fails on an existing
   path, and a path under the addon's own runtime directory belongs to a dead
   predecessor rather than to a live peer."
  [socket-path dispatch!]
  (.delete (File. ^String socket-path))
  (let [address (UnixDomainSocketAddress/of ^String socket-path)
        srv (doto (ServerSocketChannel/open StandardProtocolFamily/UNIX)
              (.bind address))
        running (atom true)
        thread (doto (Thread.
                      #(while @running (serve-one! srv dispatch!))
                      "hive-tmux-observe")
                 (.setDaemon true)
                 (.start))]
    (log/info "hive-tmux observe listening" {:socket socket-path})
    {:server srv :thread thread :running running :socket-path socket-path}))

(defn stop!
  "Close the listener started by `start!` and unlink its socket file.

   Idempotent: stopping an already-stopped listener is a no-op rather than an
   error, so addon shutdown need not track whether start succeeded."
  [{:keys [server running socket-path]}]
  (when running (reset! running false))
  (when server
    (try (.close ^ServerSocketChannel server)
         (catch Exception _ nil)))
  (when socket-path
    (.delete (File. ^String socket-path)))
  :stopped)

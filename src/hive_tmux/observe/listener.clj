(ns hive-tmux.observe.listener
  "UNIX-socket boundary: accepts hook arrivals and hands each decoded event to
   an injected dispatch fn.

   The dispatch fn is an argument rather than a direct `hive-events` call, so
   the accept loop is exercised in tests against a recording fn and carries no
   dependency on the event system it feeds.

   Binding follows STD-04 T2/T3 through hive-system.network.unix: the socket
   directory is 0700, the socket 0600, and an existing path is connect-probed
   first. A stale socket is replaced; a LIVE one is never unlinked, so a
   second listener on the same path does not start and the first keeps
   receiving.

   The adapter's bind! and close-server! take a lock file with no deadline.
   Addon init and shutdown must not hang on it, so both run on a helper
   thread with a deadline (`bind-deadline-ms`): past it the listener is left
   not started (bind), or its channel is closed without the unlink (close;
   the next bind probes the file stale and removes it)."
  (:require [hive-dsl.result :as r]
            [hive-system.network.unix :as unix]
            [hive-tmux.observe.wire :as wire]
            [taoensso.timbre :as log])
  (:import [java.nio ByteBuffer]
           [java.nio.channels ServerSocketChannel]))

(def bind-deadline-ms
  "Longest wait for the adapter's bind or close, both of which take a lock
   file with no deadline of their own."
  3000)

(defn- with-deadline
  "Run (f) on a daemon thread and wait at most MS for its value. Returns
   ::timeout when the deadline passes. Exactly one side owns the value: the
   caller when it arrives in time, else (on-late v) on the helper thread, so
   a socket bound after the deadline is closed rather than leaked."
  [ms f on-late]
  (let [p     (promise)
        owner (atom nil)]
    (doto (Thread. ^Runnable
           (fn []
             (let [v (try (f) (catch Throwable e {::thrown e}))]
               (if (compare-and-set! owner nil :caller)
                 (deliver p v)
                 (on-late v))))
           "hive-tmux-observe-bind")
      (.setDaemon true)
      (.start))
    (let [v (deref p ms ::timeout)
          v (if (and (= ::timeout v) (not (compare-and-set! owner nil :late)))
              @p
              v)]
      (if-let [e (and (map? v) (::thrown v))] (throw e) v))))

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

(defn- close-late!
  "Close a server the adapter bound after the caller stopped waiting."
  [v]
  (when (r/ok? v)
    (log/warn "hive-tmux observe: bind finished after its deadline; closing it"
              {:socket (:path (:ok v))})
    (unix/close-server! (:ok v))))

(defn start!
  "Bind SOCKET-PATH (STD-04 T3, see the ns docstring) and serve arrivals to
   DISPATCH! until stopped.

   Returns a map carrying the server and the accept thread; pass it to
   `stop!`. Returns nil, and logs why, when the listener did not start: a
   live listener already answers on the path (it is left alone), the path
   is not a socket, or the bind passed its deadline. Never unlinks a live
   socket. OPTS: :deadline-ms (default `bind-deadline-ms`)."
  ([socket-path dispatch!] (start! socket-path dispatch! {}))
  ([socket-path dispatch! {:keys [deadline-ms] :or {deadline-ms bind-deadline-ms}}]
   (let [res (with-deadline deadline-ms #(unix/bind! socket-path) close-late!)]
     (cond
       (= ::timeout res)
       (do (log/warn "hive-tmux observe: bind passed its deadline; listener not started"
                     {:socket socket-path :deadline-ms deadline-ms})
           nil)

       (r/err? res)
       (do (log/warn "hive-tmux observe: listener not started"
                     {:socket socket-path :error (:error res) :message (:message res)})
           nil)

       :else
       (let [bound   (:ok res)
             srv     (:server-socket bound)
             running (atom true)
             thread  (doto (Thread.
                            #(while @running (serve-one! srv dispatch!))
                            "hive-tmux-observe")
                       (.setDaemon true)
                       (.start))]
         (log/info "hive-tmux observe listening"
                   {:socket (:path bound) :replaced-stale? (:replaced-stale? bound)})
         {:server srv :thread thread :running running :socket-path (:path bound)
          :bound bound})))))

(defn stop!
  "Close the listener started by `start!` and unlink its socket file, but
   only while the file is still this listener's socket (a later listener's
   live socket on the same path is left alone).

   Idempotent: stopping an already-stopped (or never-started, nil) listener
   is a no-op rather than an error, so addon shutdown need not track whether
   start succeeded. The unlink runs under the adapter's lock with a
   deadline; past it the channel is closed and the file left for the next
   bind to probe stale."
  [{:keys [server running bound] :as l}]
  (when running (reset! running false))
  (when (and l server)
    (when (= ::timeout (with-deadline bind-deadline-ms
                         #(unix/close-server! bound)
                         (constantly nil)))
      (log/warn "hive-tmux observe: close passed its deadline; socket file left"
                {:socket (:socket-path l)}))
    (try (.close ^ServerSocketChannel server)
         (catch Exception _ nil)))
  :stopped)

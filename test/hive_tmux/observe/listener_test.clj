(ns hive-tmux.observe.listener-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-system.network.unix :as unix]
            [hive-tmux.observe.listener :as listener]
            [hive-tmux.observe.wire :as wire])
  (:import [java.io File]
           [java.net StandardProtocolFamily UnixDomainSocketAddress]
           [java.nio.channels SocketChannel]
           [java.nio ByteBuffer]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- tmp-socket-path
  "A socket path in a fresh private directory: the adapter refuses a socket
   directory this process does not own, such as /tmp itself. The leaf
   directory does not exist yet, so the listener creates it (0700)."
  []
  (let [base (Files/createTempDirectory "hto-" (make-array FileAttribute 0))]
    (str base "/d/tmux-observe.sock")))

(defn- send! [path ^String payload]
  (with-open [ch (SocketChannel/open StandardProtocolFamily/UNIX)]
    (.connect ch (UnixDomainSocketAddress/of ^String path))
    (.write ch (ByteBuffer/wrap (.getBytes payload "UTF-8")))))

(defn- await-count [a n]
  (let [deadline (+ (System/currentTimeMillis) 3000)]
    (while (and (< (count @a) n) (< (System/currentTimeMillis) deadline))
      (Thread/sleep 10))
    @a))

(deftest arrivals-reach-the-injected-dispatch
  (let [path (tmp-socket-path)
        seen (atom [])
        l (listener/start! path #(swap! seen conj %))]
    (try
      (let [e (wire/args->event ["pane-died" "%14"] 1 "tmuxenv")]
        (send! path (wire/encode e))
        (testing "the decoded event, not the bytes, reaches dispatch"
          (is (= [e] (await-count seen 1)))))
      (finally (listener/stop! l)))))

(deftest a-bad-arrival-does-not-end-the-loop
  (let [path (tmp-socket-path)
        seen (atom [])
        l (listener/start! path #(swap! seen conj %))]
    (try
      (send! path "{{garbage")
      (let [good (wire/args->event ["client-attached" "%2"] 2 nil)]
        (send! path (wire/encode good))
        (testing "the listener survives an undecodable payload and serves the next"
          (is (= [good] (await-count seen 1)))))
      (finally (listener/stop! l)))))

(deftest stop-is-idempotent-and-unlinks
  (let [path (tmp-socket-path)
        l (listener/start! path (fn [_]))]
    (listener/stop! l)
    (testing "the socket file is removed so a restart can bind"
      (is (not (.exists (File. path)))))
    (testing "stopping twice is a no-op rather than an error"
      (is (= :stopped (listener/stop! l))))
    (testing "stopping a listener that never started is a no-op too"
      (is (= :stopped (listener/stop! nil))))))

(deftest directory-is-0700-and-socket-is-0600
  (let [path (tmp-socket-path)
        l (listener/start! path (fn [_]))]
    (try
      (is (some? l))
      (is (= {:ok "rwx------"} (unix/mode-string (.getParent (File. path)))))
      (is (= {:ok "rw-------"} (unix/mode-string path)))
      (finally (listener/stop! l)))))

(deftest start-replaces-a-stale-socket-file
  (let [path (tmp-socket-path)
        l1 (listener/start! path (fn [_]))]
    ;; a dead predecessor: its channel is closed but its socket file remains
    (reset! (:running l1) false)
    (.close (:server l1))
    (is (.exists (File. path)) "precondition: the stale file is still there")
    (is (= {:ok :stale} (unix/probe path)))
    (testing "a leftover file from a dead predecessor does not block bind"
      (let [seen (atom [])
            l2 (listener/start! path #(swap! seen conj %))]
        (try
          (is (some? l2))
          (is (true? (:replaced-stale? (:bound l2))))
          (let [e (wire/args->event ["pane-exited" "%3"] 3 nil)]
            (send! path (wire/encode e))
            (is (= [e] (await-count seen 1))))
          (finally (listener/stop! l2)))))))

(deftest a-live-socket-is-not-replaced
  (let [path   (tmp-socket-path)
        first* (atom [])
        second* (atom [])
        l1 (listener/start! path #(swap! first* conj %))]
    (try
      (let [l2 (listener/start! path #(swap! second* conj %))]
        (testing "the second listener does not start on a live path"
          (is (nil? l2)))
        (testing "and the first keeps receiving on the same socket file"
          (let [e (wire/args->event ["pane-died" "%9"] 9 nil)]
            (send! path (wire/encode e))
            (is (= [e] (await-count first* 1)))
            (is (= [] @second*))))
        (testing "stopping the refused one (nil) leaves the live socket alone"
          (listener/stop! l2)
          (is (= {:ok :live} (unix/probe path)))))
      (finally (listener/stop! l1)))))

(deftest with-deadline-hands-a-late-value-to-on-late
  (let [with-deadline #'listener/with-deadline
        gate (promise)
        late (promise)]
    (testing "a value later than the deadline answers ::timeout"
      (is (= :hive-tmux.observe.listener/timeout
             (with-deadline 50 (fn [] @gate) #(deliver late %)))))
    (testing "and the value, once produced, goes to on-late (the late close)"
      (deliver gate {:ok :bound-late})
      (is (= {:ok :bound-late} (deref late 3000 ::never))))))

(deftest with-deadline-returns-an-in-time-value-and-never-calls-on-late
  (let [with-deadline #'listener/with-deadline
        late (promise)]
    (is (= {:ok :bound} (with-deadline 3000 (fn [] {:ok :bound}) #(deliver late %))))
    (is (= ::not-called (deref late 100 ::not-called)))))

(deftest a-non-socket-path-is-left-alone
  (let [path (tmp-socket-path)
        f    (File. path)]
    (.mkdirs (.getParentFile f))
    (spit f "operator data")
    (is (nil? (listener/start! path (fn [_]))))
    (is (= "operator data" (slurp f)))))

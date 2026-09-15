(ns hive-tmux.observe.listener-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.observe.listener :as listener]
            [hive-tmux.observe.wire :as wire])
  (:import [java.io File]
           [java.net StandardProtocolFamily UnixDomainSocketAddress]
           [java.nio.channels SocketChannel]
           [java.nio ByteBuffer]))

(defn- tmp-socket-path []
  (str (System/getProperty "java.io.tmpdir") "/hive-tmux-observe-test-"
       (System/nanoTime) ".sock"))

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
      (is (= :stopped (listener/stop! l))))))

(deftest start-replaces-a-stale-socket-file
  (let [path (tmp-socket-path)
        l1 (listener/start! path (fn [_]))]
    (.close (:server l1))
    (testing "a leftover file from a dead predecessor does not block bind"
      (let [seen (atom [])
            l2 (listener/start! path #(swap! seen conj %))]
        (try
          (let [e (wire/args->event ["pane-exited" "%3"] 3 nil)]
            (send! path (wire/encode e))
            (is (= [e] (await-count seen 1))))
          (finally (listener/stop! l2)))))))

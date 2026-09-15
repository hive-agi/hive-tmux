(ns hive-tmux.observe.wire-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.observe.wire :as wire]))

(deftest classification-table
  (testing "known hooks map to the hive-events id they stand for"
    (is (= :tmux/pane-killed (wire/classify "pane-died")))
    (is (= :tmux/pane-killed (wire/classify "pane-exited")))
    (is (= :tmux/state-changed (wire/classify "client-attached"))))
  (testing "an unknown hook is classified, never guessed into an existing id"
    (is (= :tmux/unknown-hook (wire/classify "hook-tmux-adds-in-3.6")))
    (is (= :tmux/unknown-hook (wire/classify nil)))))

(deftest roundtrip-is-total-over-events
  (testing "every event survives encode -> decode unchanged"
    (doseq [hook (keys wire/hook->event-id)]
      (let [e (wire/args->event [hook "%14"] 1789509295960 "/tmp/tmux-1000/default,1,0")]
        (is (= e (wire/decode (wire/encode e))) (str "roundtrip " hook))))))

(deftest decode-rejects-rather-than-evaluates
  (testing "a hostile or malformed payload is nil, not a code path"
    (is (nil? (wire/decode "{{not edn")))
    (is (nil? (wire/decode "#=(java.lang.Runtime/getRuntime)")))
    (is (nil? (wire/decode "42")) "a non-map is not an event")
    (is (nil? (wire/decode "")))))

(deftest args-event-is-pure-over-clock-and-env
  (testing "clock and env are arguments, so the event is reproducible"
    (is (= (wire/args->event ["pane-died" "%1"] 7 "env")
           (wire/args->event ["pane-died" "%1"] 7 "env"))))
  (testing "a missing pane is carried as nil rather than dropped"
    (let [e (wire/args->event ["pane-died"] 7 nil)]
      (is (nil? (:pane e)))
      (is (= :tmux/pane-killed (:event e))))))

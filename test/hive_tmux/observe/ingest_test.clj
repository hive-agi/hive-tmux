(ns hive-tmux.observe.ingest-test
  "The arrival path against recording ports: the registry action precedes the
   event dispatch, and a faulting handler cannot kill the accept loop."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.observe.ingest :as ingest]
            [hive-tmux.observe.wire :as wire]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- ports [registry-atom log]
  {:registry    (fn [] @registry-atom)
   :deregister! (fn [ling-id]
                  (swap! registry-atom dissoc ling-id)
                  (swap! log conj [:deregister ling-id]))
   :dispatch!   (fn [v] (swap! log conj [:dispatch v]))})

(deftest externally-killed-pane-is-deregistered
  (let [reg (atom {"ling-a" {:pane-id "%1"}})
        log (atom [])
        p (ingest/handle (ports reg log) (wire/args->event ["pane-died" "%1"] 1 nil))]
    (testing "the ling leaves the registry"
      (is (= {:action :deregister :ling-id "ling-a" :pane "%1"} p))
      (is (= {} @reg)))
    (testing "the registry action happens BEFORE the event is dispatched"
      (is (= :deregister (ffirst @log))
          "a handler reading the registry must not see the dead pane"))
    (testing "the dispatched event carries the pane, ling and its tmux-hook origin"
      (let [[_ [event-id payload]] (second @log)]
        (is (= :tmux/pane-killed event-id))
        (is (= "%1" (:pane-id payload)))
        (is (= "ling-a" (:ling-id payload)))
        (is (= :tmux-hook (:source payload))
            "source distinguishes an observed death from one hive caused")))))

(deftest a-state-change-dispatches-without-touching-the-registry
  (let [reg (atom {"ling-a" {:pane-id "%1"}})
        log (atom [])]
    (ingest/handle (ports reg log) (wire/args->event ["client-attached" "%1"] 1 nil))
    (is (= {"ling-a" {:pane-id "%1"}} @reg) "a live pane survives a state change")
    (is (= 1 (count @log)))
    (is (= :dispatch (ffirst @log)))))

(deftest a-faulting-handler-does-not-propagate
  (let [boom {:registry (fn [] (throw (ex-info "registry down" {})))
              :deregister! (fn [_])
              :dispatch! (fn [_])}
        f (ingest/dispatch-fn boom)]
    (testing "the listener keeps serving; the fault is a logged plan"
      (is (= {:action :ignore :reason :handler-failed}
             (f (wire/args->event ["pane-died" "%1"] 1 nil)))))))

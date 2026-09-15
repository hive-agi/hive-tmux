(ns hive-tmux.observe.reconcile-test
  "Reconciling an arrival against the registry: the reverse pane-id lookup and
   the decision table. Pure over a registry value, so no registry, tmux server
   or socket is involved."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.observe.reconcile :as rec]
            [hive-tmux.observe.wire :as wire]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def registry
  {"ling-a" {:pane-id "%1" :cwd "/a"}
   "ling-b" {:pane-id "%2" :cwd "/b"}})

(defn- death [pane] (wire/args->event ["pane-died" pane] 1 nil))

(deftest reverse-lookup
  (testing "a registered pane resolves to its ling"
    (is (= "ling-a" (rec/ling-for-pane registry "%1")))
    (is (= "ling-b" (rec/ling-for-pane registry "%2"))))
  (testing "an unregistered or absent pane resolves to nil"
    (is (nil? (rec/ling-for-pane registry "%99")))
    (is (nil? (rec/ling-for-pane registry nil)))
    (is (nil? (rec/ling-for-pane {} "%1"))))
  (testing "a pane claimed by two lings resolves deterministically"
    (let [dup {"ling-z" {:pane-id "%1"} "ling-a" {:pane-id "%1"}}]
      (is (= "ling-a" (rec/ling-for-pane dup "%1")))
      (is (= (rec/ling-for-pane dup "%1") (rec/ling-for-pane dup "%1"))))))

(deftest only-a-death-deregisters
  (testing "a pane death for a registered pane deregisters its ling"
    (is (= {:action :deregister :ling-id "ling-a" :pane "%1"}
           (rec/plan registry (death "%1")))))
  (testing "a death for a pane hive never registered is ignored"
    (is (= :pane-not-registered (:reason (rec/plan registry (death "%99"))))))
  (testing "a state change is not a death and must not evict a live pane"
    (let [e (wire/args->event ["client-attached" "%1"] 1 nil)]
      (is (= {:action :ignore :reason :not-a-death} (rec/plan registry e)))))
  (testing "an unknown hook is ignored as unknown, not treated as a death"
    (let [e (wire/args->event ["hook-from-a-future-tmux" "%1"] 1 nil)]
      (is (= {:action :ignore :reason :unknown-hook} (rec/plan registry e))))))

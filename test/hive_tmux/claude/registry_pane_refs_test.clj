(ns hive-tmux.claude.registry-pane-refs-test
  "Every pane-ref list parameter (pane, exclude) goes through one normalizer,
   `registry/pane-refs`. `watch start exclude=[\"%9\"]` once stored the
   vector's printed text and excluded nothing (card 20261120021807-1a6075cf)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.compact-test :as ct]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.registry :as registry]
            [hive-tmux.claude.watch :as watch]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(deftest a-json-array-as-data
  (is (= ["%9"] (registry/pane-refs ["%9"])))
  (is (= ["%9" "peer one"] (registry/pane-refs [" %9 " "peer one" ""]))))

(deftest a-json-array-as-text
  (is (= ["%9"] (registry/pane-refs "[\"%9\"]")))
  (is (= ["%9" "1:3.1"] (registry/pane-refs " [\"%9\", \"1:3.1\"] "))))

(deftest a-comma-separated-string
  (is (= ["%9" "1:3.1" "peer one"] (registry/pane-refs "%9, 1:3.1 ,peer one")))
  (is (= ["%9"] (registry/pane-refs "%9,,"))))

(deftest a-single-ref
  (is (= ["%9"] (registry/pane-refs "%9")))
  (is (= ["peer one"] (registry/pane-refs "peer one"))))

(deftest every-shape-gives-the-same-vector
  (is (apply = (map registry/pane-refs [["%9"] "[\"%9\"]" "%9" "%9," #{"%9"}]))))

(deftest nothing-is-nil
  (doseq [x [nil "" "  " [] "[]" ","]]
    (is (nil? (registry/pane-refs x)) (pr-str x))))

(deftest a-bracket-that-is-not-a-json-array-of-strings-is-refused
  (doseq [x ["[%9]" "[1, 2]" "[\"%9\""]]
    (testing x
      (is (= :tmux/bad-pane-refs
             (:type (ex-data (try (registry/pane-refs x) (catch Exception e e)))))))))

(deftest watch-start-stores-the-refs-not-the-vector-text
  (let [{:keys [host]} (ct/scripted-host {:tokens 1000 :goal? false})]
    (binding [host/*host* host]
      (try
        (doseq [x [["%9"] "[\"%9\"]" "%9"]]
          (testing (pr-str x)
            (let [{:keys [ok]} (registry/dispatch {:command "watch start" :parent "coordinator:1" :exclude x
                                                   :interval_ms 60000})]
              (is (= ["%9"] (get-in ok [:opts :exclude]))))))
        (finally (watch/stop!))))))

(deftest compact-sweep-honours-an-exclude-array
  (let [{:keys [host]} (ct/scripted-host {:tokens 450000 :goal? false})]
    (binding [host/*host* host]
      (doseq [x [["%9"] "[\"%9\"]" "%9" "peer one"]]
        (testing (pr-str x)
          (is (empty? (get-in (registry/dispatch {:command "compact" :dry_run true :exclude x})
                              [:ok :would-compact]))))))))

(deftest compact-pane-takes-an-array
  (let [{:keys [host]} (ct/scripted-host {:tokens 450000 :goal? false})]
    (binding [host/*host* host]
      (is (= ["%9"] (mapv :pane (get-in (registry/dispatch {:command "compact" :dry_run true :pane ["%9"]})
                                        [:ok :would-compact])))))))

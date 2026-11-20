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

(defn- refs [x] (:ok (registry/pane-refs x)))

(deftest a-json-array-as-data
  (is (= ["%9"] (refs ["%9"])))
  (is (= ["%9" "peer one"] (refs [" %9 " "peer one" ""]))))

(deftest a-json-array-as-text
  (is (= ["%9"] (refs "[\"%9\"]")))
  (is (= ["%9" "%5"] (refs "[\"%9\", \"%5\"]")) "two refs, JSON form")
  (is (= ["%9" "1:3.1"] (refs " [\"%9\",\"1:3.1\"] "))))

(deftest a-printed-vector-of-two-refs
  ;; What (str ["%9" "%5"]) gives: no comma, which strict JSON refuses.
  (is (= "[\"%9\" \"%5\"]" (str ["%9" "%5"])))
  (is (= ["%9" "%5"] (refs (str ["%9" "%5"])))))

(deftest a-comma-separated-string
  (is (= ["%9" "1:3.1" "peer one"] (refs "%9, 1:3.1 ,peer one")))
  (is (= ["%9"] (refs "%9,,"))))

(deftest a-single-ref
  (is (= ["%9"] (refs "%9")))
  (is (= ["peer one"] (refs "peer one"))))

(deftest every-shape-gives-the-same-vector
  (is (apply = (map refs [["%9"] "[\"%9\"]" "%9" "%9," #{"%9"}]))))

(deftest nothing-is-nil
  (doseq [x [nil "" "  " [] "[]" ","]]
    (is (= {:ok nil} (registry/pane-refs x)) (pr-str x))))

(deftest a-bracket-that-is-not-an-array-of-strings-is-an-error-result
  (doseq [x ["[%9]" "[1, 2]" "[\"%9\"" "[:a]"]]
    (testing x
      (let [r (registry/pane-refs x)]
        (is (= :tmux/bad-pane-refs (:error r)))
        (is (= x (:value r)))))))

(deftest a-bad-ref-list-is-reported-not-thrown
  (let [{:keys [host]} (ct/scripted-host {:tokens 450000 :goal? false})]
    (binding [host/*host* host]
      (doseq [[cmd param] [["compact" :exclude] ["compact" :pane] ["watch start" :exclude]]]
        (testing (str cmd " " param)
          (let [r (registry/dispatch {:command cmd :parent "coordinator:1" :dry_run true param "[%9]"})]
            (is (= :tmux/bad-pane-refs (:error r)) (pr-str r))
            (is (= param (:param r)))))))
    (watch/stop!)))

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
      (doseq [x [["%9"] "[\"%9\"]" (str ["%9" "%5"]) "%9" "peer one"]]
        (testing (pr-str x)
          (is (empty? (get-in (registry/dispatch {:command "compact" :dry_run true :exclude x})
                              [:ok :would-compact]))))))))

(deftest compact-pane-takes-an-array
  (let [{:keys [host]} (ct/scripted-host {:tokens 450000 :goal? false})]
    (binding [host/*host* host]
      (is (= ["%9"] (mapv :pane (get-in (registry/dispatch {:command "compact" :dry_run true :pane ["%9"]})
                                        [:ok :would-compact])))))))

(deftest the-schema-declares-ref-lists-as-array-or-string
  ;; A param declared string-only invites a host to stringify the array.
  (doseq [k ["pane" "exclude"]]
    (is (= [{:type "array" :items {:type "string"}} {:type "string"}]
           (get-in registry/schema-ext [k :anyOf]))
        k)
    (is (nil? (get-in registry/schema-ext [k :type])) k)))

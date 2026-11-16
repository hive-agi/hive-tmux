(ns hive-tmux.claude.registry-own-pane-test
  "`swarm tmux compact` never compacts the caller's own pane: a compact job
   waits for a settled idle, which the caller's own turn (held open by a
   /goal Stop hook) never gives it. Against the scripted host of
   `compact-test`, whose one pane %9 shows the session of pid 200."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.compact-test :as ct]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.registry :as registry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private me "coordinator:200")

(deftest caller-pid-reads-only-a-coordinator-id
  (is (= 200 (registry/caller-pid me)))
  (is (nil? (registry/caller-pid "coordinator:")))
  (is (nil? (registry/caller-pid "coordinator:0")))
  (is (nil? (registry/caller-pid "coordinator:12ab")))
  (is (nil? (registry/caller-pid "ling-7:200")))
  (is (nil? (registry/caller-pid nil))))

(deftest naming-your-own-pane-is-refused-at-once
  ;; Friction 20261112125107-63cb6566: four compacts of the caller's own pane
  ;; under a /goal Stop hook each waited 30 min and timed out.
  (let [{:keys [state host]} (ct/scripted-host {:tokens 450000 :goal? true})
        jobs-before (count (compact/jobs))]
    (binding [host/*host* host]
      (doseq [ref ["%9" "peer one" "%9,peer one"]]
        (testing ref
          (let [r (registry/dispatch {:command "compact" :pane ref :_caller_id me})]
            (is (= :tmux/own-pane (:error r)))
            (is (= ["%9"] (mapv :pane (:own-pane r))))
            (is (= "swarm steer self input=/compact then=<reseed> dry_run=false" (:use r)))
            (is (string? (:why r)))))))
    (is (= jobs-before (count (compact/jobs))) "no job was started")
    (is (not-any? #(= "send-keys" (first %)) (:argv @state)) "not one key was sent")))

(deftest a-sweep-leaves-your-own-pane-out
  (let [{:keys [state host]} (ct/scripted-host {:tokens 450000 :goal? false})]
    (binding [host/*host* host]
      (testing "dry run"
        (let [{:keys [ok]} (registry/dispatch {:command "compact" :dry_run true :_caller_id me})]
          (is (empty? (:would-compact ok)))
          (is (= ["%9"] (mapv :pane (get-in ok [:skipped-own-pane :own-pane]))))
          (is (some? (get-in ok [:skipped-own-pane :use])))))
      (testing "for real"
        (let [{:keys [ok]} (registry/dispatch {:command "compact" :_caller_id me})]
          (is (empty? (:started ok)))
          (is (some? (:skipped-own-pane ok))))))
    (is (empty? (:sent @state)))))

(deftest another-callers-pane-is-compacted-as-before
  (let [{:keys [host]} (ct/scripted-host {:tokens 450000 :goal? false})]
    (binding [host/*host* host]
      (doseq [caller ["coordinator:201" "ling-7:200" nil]]
        (testing (pr-str caller)
          (let [sweep (registry/dispatch {:command "compact" :dry_run true :_caller_id caller})
                named (registry/dispatch {:command "compact" :pane "%9" :dry_run true :_caller_id caller})]
            (is (= ["%9"] (mapv :pane (get-in sweep [:ok :would-compact]))))
            (is (nil? (get-in sweep [:ok :skipped-own-pane])))
            (is (= ["%9"] (mapv :pane (get-in named [:ok :would-compact]))))))))))

(ns hive-tmux.claude.compact-cancel-test
  "Cancelling a job that has not typed yet (card 20261120021807-1a6075cf,
   friction 20261120014457-60a1729a). The decision is pure over the job value;
   the boundary is driven against the scripted host of `compact-test`, whose
   pane %9 is held busy so a job waits there, never typing."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.compact-test :as ct]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.registry :as registry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;;; Pure

(deftest the-cancel-verdict-reads-only-the-job-value
  (is (= {:cancel? true} (compact/cancel-verdict {:status :queued})))
  (is (= {:cancel? true} (compact/cancel-verdict {:status :running :steps [{:step :wait-idle}]})))
  (is (= {:refuse :already-typed :step :compact}
         (compact/cancel-verdict {:status :running :typed-step :compact})))
  (is (= {:refuse :not-active :status :done} (compact/cancel-verdict {:status :done})))
  (is (= {:refuse :already-cancelled} (compact/cancel-verdict {:status :cancelled :cancelled-at 1}))))

(deftest cancel-job-and-claim-typing-exclude-each-other
  (let [j {:job-id "cc-1" :status :running}
        cancelled (compact/cancel-job j :cancelled 42)
        typed (compact/claim-typing j :say)]
    (is (= :cancelled (:status cancelled)))
    (is (= 42 (:cancelled-at cancelled) (:ended-at cancelled)))
    (is (= cancelled (compact/claim-typing cancelled :say)) "a cancelled job does not claim a keystroke")
    (is (= :say (:typed-step typed)))
    (is (= typed (compact/cancel-job typed :cancelled 42)) "a job that typed is not cancelled")
    (is (= :say (:typed-step (compact/claim-typing typed :goal-set))) "the FIRST step that typed is kept")))

(deftest a-cancelled-job-stays-cancelled
  (is (compact/job-consistent? {:status :running}))
  (is (compact/job-consistent? {:status :cancelled :cancelled-at 1}))
  (is (not (compact/job-consistent? {:status :failed :cancelled-at 1}))))

;;; Boundary

(defn held-busy-host
  "The scripted host with %9 busy for good and a clock that never moves: a job
   waits on it until cancelled, sleeping 2 ms per poll for real."
  []
  (let [{:keys [state host]} (ct/scripted-host {:tokens 450000 :goal? false})]
    (swap! state assoc :busy-polls Long/MAX_VALUE)
    {:state state :host (assoc host :sleep! (fn [_] (Thread/sleep 2)))}))

(defn await-job [job-id pred]
  (loop [n 0]
    (let [j (compact/job job-id)]
      (if (or (pred j) (> n 500)) j (do (Thread/sleep 5) (recur (inc n)))))))

(deftest an-untyped-job-is-cancelled-frees-the-slot-and-ends-once
  (let [{:keys [state host]} (held-busy-host)
        ended (atom [])
        job (compact/start! "%9" {:host host :kind :say :text "stale wrap order"
                                  :poll-ms 1 :settle-ms 1 :on-end #(swap! ended conj %)})
        id (:job-id job)]
    (await-job id #(seq (:steps %)))
    (let [res (compact/cancel! id)]
      (is (true? (:cancelled? res)))
      (is (= :cancelled (get-in res [:job :status]))))
    (testing "the runner stops at its next wait and cannot overwrite the status"
      (Thread/sleep 50)
      (is (= :cancelled (:status (compact/job id))))
      (is (= 1 (count @ended)) "on-end ran once, from the cancel")
      (is (= :cancelled (:status (first @ended)))))
    (is (empty? (:sent @state)) "not one line was submitted")
    (is (not-any? #(= "send-keys" (first %)) (:argv @state)) "not one key was sent")
    (testing "the slot is free: a second job starts on the same pane"
      (let [next-job (compact/start! "%9" {:host host :kind :say :text "x" :poll-ms 1 :settle-ms 1})]
        (is (nil? (:error next-job)) (pr-str next-job))
        (compact/cancel! (:job-id next-job))))
    (is (= :already-cancelled (:refuse (compact/cancel! id))))))

(deftest a-job-that-typed-refuses-naming-the-step
  (let [{:keys [host]} (ct/scripted-host {:tokens 450000 :goal? false})
        job (compact/start! "%9" (assoc ct/fast :host host :kind :say :text "hello"))
        res (compact/cancel! (:job-id job))]
    (is (= :done (:status job)))
    (is (= :say (:typed-step job)))
    (is (false? (:cancelled? res)))
    (is (= :already-typed (:refuse res)))
    (is (= :say (:step res)))
    (is (= :done (:status (compact/job (:job-id job)))) "the job is left as it was")))

(deftest the-job-verb-cancels-and-refuses
  (let [{:keys [host]} (held-busy-host)]
    (binding [host/*host* host]
      (let [{started :ok} (registry/dispatch {:command "say" :pane "%9" :text "stale"})
            id (:job-id started)]
        (await-job id #(seq (:steps %)))
        (testing "cancel=true as a string, the way the JSON surface may send it"
          (let [r (registry/dispatch {:command "job" :job_id id :cancel "true"})]
            (is (= :cancelled (get-in r [:ok :status])) (pr-str r))))
        (is (= :tmux/already-cancelled (:error (registry/dispatch {:command "job" :job_id id :cancel true}))))
        (is (= :tmux/no-such-job (:error (registry/dispatch {:command "job" :job_id "cc-nope" :cancel true}))))
        (is (= :tmux/no-job-id (:error (registry/dispatch {:command "job" :cancel true})))))))
  (let [{:keys [host]} (ct/scripted-host {:tokens 450000 :goal? false})
        job (compact/start! "%9" (assoc ct/fast :host host :kind :say :text "hello"))
        r (registry/dispatch {:command "job" :job_id (:job-id job) :cancel true})]
    (is (= :tmux/already-typed (:error r)))
    (is (= :say (:step r)))
    (is (re-find #"already typed its say step" (:hint r)))))

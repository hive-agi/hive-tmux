(ns hive-tmux.claude.compact-cancel-test
  "Cancelling a job that has not typed yet, and superseding one (card
   20261120021807-1a6075cf, friction 20261120014457-60a1729a). The decision
   is pure over the job value; the boundary is driven against the scripted
   host of `compact-test`. Every wait here has a deadline, and a test only
   reads the jobs it created."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.compact-test :as ct]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.registry :as registry]
            [hive-tmux.claude.watch :as watch]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;;; Helpers

(defn- await-until
  "The first truthy (PRED), polled until TIMEOUT-MS has passed; nil after."
  [pred timeout-ms]
  (let [end (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (or (pred)
          (when (< (System/currentTimeMillis) end)
            (Thread/sleep 2)
            (recur))))))

(defn- await-job [id pred] (await-until #(let [j (compact/job id)] (when (pred j) j)) 2000))

(defn- held-busy-host
  "The scripted host with %9 busy for good: a job waits on it, never typing,
   sleeping 2 ms per poll for real, until it is cancelled."
  ([] (held-busy-host 450000))
  ([tokens]
   (let [{:keys [state host]} (ct/scripted-host {:tokens tokens :goal? false})]
     (swap! state assoc :busy-polls Long/MAX_VALUE)
     {:state state :host (assoc host :sleep! (fn [_] (Thread/sleep 2)))})))

(defn- start-held!
  "Start a KIND job on %9 of HOST in its own thread, synchronously there, so
   the returned :run (a future) completes exactly when the runner has ended.
   {:id :run}, :id being the one job this call created."
  [host opts]
  (let [before (set (map :job-id (compact/jobs)))
        run (future (compact/start! "%9" (assoc opts :host host :sync? true :poll-ms 1 :settle-ms 1)))
        id (await-until #(first (remove before (map :job-id (compact/jobs)))) 2000)]
    {:id id :run run}))

(defn- active? [id] (contains? #{:queued :running} (:status (compact/job id))))

(defn- latched
  "HOST whose first keystroke (the first load-buffer, paste-buffer or
   send-keys, i.e. inside `composer/send-text!`, after `deliver!` claimed it)
   blocks until :release is delivered; :entered is delivered when it blocks.
   The wait is bounded, so a failing test cannot leave a runner stuck."
  [host]
  (let [entered (promise) release (promise) armed (atom true) tmux! (:tmux! host)]
    {:entered entered :release release
     :host (assoc host :tmux! (fn latched-tmux
                                ([argv] (latched-tmux argv nil))
                                ([argv stdin]
                                 (when (and (#{"load-buffer" "paste-buffer" "send-keys"} (first argv))
                                            (compare-and-set! armed true false))
                                   (deliver entered true)
                                   (deref release 5000 nil))
                                 (tmux! argv stdin))))}))

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

(deftest start-refusal-names-a-job-that-would-stop-at-once
  (let [idle {:ctx-tokens 450000 :busy? false}
        busy {:ctx-tokens 450000 :busy? true}]
    (is (nil? (compact/start-refusal :compact {} idle)))
    (is (= :under-threshold (:reason (compact/start-refusal :compact {} {:ctx-tokens 1000}))))
    (is (nil? (compact/start-refusal :compact {:force? true} {:ctx-tokens 1000})))
    (is (= :busy (:reason (compact/start-refusal :say {:text "x" :wait-idle? false} busy))))
    (is (nil? (compact/start-refusal :say {:text "x"} busy)) "a say that waits for idle will run")
    (is (= :no-text (:reason (compact/start-refusal :say {:text " "} idle))))
    (is (= :no-such-pane (:reason (compact/start-refusal :goal {:goal "g"} nil))))))

;;; Cancel

(deftest an-untyped-job-is-cancelled-frees-the-slot-and-ends-once
  (let [{:keys [state host]} (held-busy-host)
        ended (atom [])
        {:keys [id run]} (start-held! host {:kind :say :text "stale wrap order" :on-end #(swap! ended conj %)})]
    (is (some? (await-job id #(seq (:steps %)))) "the job is waiting on the busy pane")
    (let [res (compact/cancel! id)]
      (is (true? (:cancelled? res)))
      (is (= :cancelled (get-in res [:job :status]))))
    (testing "the runner stops at its next wait and cannot overwrite the status"
      (is (= :cancelled (:status (deref run 2000 ::still-running))))
      (is (= :cancelled (:status (compact/job id))))
      (is (= [:cancelled] (mapv :status @ended)) "on-end ran once, from the cancel"))
    (is (empty? (:sent @state)) "not one line was submitted")
    (is (not-any? #(= "send-keys" (first %)) (:argv @state)) "not one key was sent")
    (testing "the slot is free: a second job starts on the same pane"
      (let [{id2 :id run2 :run} (start-held! host {:kind :say :text "x"})]
        (is (active? id2))
        (compact/cancel! id2)
        (is (= :cancelled (:status (deref run2 2000 ::still-running))))))
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
        (is (some? (await-job id #(seq (:steps %)))))
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

(deftest the-sense-of-a-job-cancelled-before-it-ran-names-the-pane
  (let [heard (atom [])
        {:keys [host]} (ct/scripted-host {:tokens 1000 :goal? false})]
    (watch/start! {:parent "coordinator:1" :host host :interval-ms 60000
                   :shout-fn (fn [_ event data] (swap! heard conj [event data]))})
    (try
      (watch/job-ended! {:kind :say :job-id "cc-x" :pane "%9" :status :cancelled :reason :cancelled})
      (let [[[event data]] @heard]
        (is (= :tmux-job event))
        (is (= "say cc-x on %9: cancelled cancelled" (:message data))))
      (finally (watch/stop!)))))

;;; Supersede

(def ^:private long-text (apply str "wrap now: " (repeat 20 "0123456789")))

(deftest blocking-view-names-the-job-its-kind-and-80-chars
  (let [v (compact/blocking-view {:job-id "cc-1" :kind :say :status :running :opts {:text long-text}})]
    (is (= {:job-id "cc-1" :kind :say :status :running :text (subs long-text 0 80)} v)))
  (is (= "/goal ship it" (:text (compact/blocking-view {:kind :goal :opts {:goal "ship it"}}))))
  (is (= "/compact keep the plan" (:text (compact/blocking-view {:kind :compact :opts {:instructions "keep the plan"}}))))
  (is (= :say (:typed-step (compact/blocking-view {:kind :say :typed-step :say :opts {:text "x"}})))))

(deftest without-supersede-the-refusal-names-the-blocker
  (let [{:keys [host]} (held-busy-host)
        {:keys [id run]} (start-held! host {:kind :say :text long-text})
        b (compact/start! "%9" {:host host :kind :compact})]
    (is (= :already-running (:error b)))
    (is (= {:job-id id :kind :say :text (subs long-text 0 80)}
           (select-keys (:blocking b) [:job-id :kind :text])))
    (is (re-find #"supersede=true" (:hint b)))
    (compact/cancel! id)
    (is (= :cancelled (:status (deref run 2000 ::still-running))))))

(deftest supersede-cancels-an-untyped-job-and-takes-the-slot
  (let [{:keys [state host]} (held-busy-host)
        ended (atom [])
        {first-id :id} (start-held! host {:kind :say :text "stale" :on-end #(swap! ended conj %)})]
    (is (some? (await-job first-id #(seq (:steps %)))))
    (binding [host/*host* host]
      (loop [blocker first-id
             [[verb extra] & more] [["compact" {:pane "%9"}] ["say" {:pane "%9" :text "new"}] ["goal" {:pane "%9" :goal "g"}]]]
        (let [r (registry/dispatch (merge {:command verb :supersede "true"} extra))
              started (or (first (get-in r [:ok :started])) (:ok r))]
          (testing verb
            (is (nil? (:error started)) (pr-str r))
            (is (= blocker (:superseded started)))
            (is (= :cancelled (:status (compact/job blocker))))
            (is (= :superseded (:reason (compact/job blocker)))))
          (if (seq more)
            (recur (:job-id started) more)
            (do (compact/cancel! (:job-id started))
                (is (some? (await-job (:job-id started) #(= :cancelled (:status %))))))))))
    (is (= [:cancelled] (mapv :status @ended)) "the superseded job fired its sense once")
    (is (empty? (:sent @state)))))

(deftest supersede-never-cancels-a-job-that-typed
  ;; The real claim in `deliver!`: the say is held INSIDE its first keystroke.
  (let [{:keys [state host]} (ct/scripted-host {:tokens 450000 :goal? false})
        {:keys [entered release] latched-host :host} (latched host)
        job (compact/start! "%9" {:host latched-host :kind :say :text "typing" :poll-ms 1000 :settle-ms 2000})
        id (:job-id job)]
    (try
      (is (true? (deref entered 2000 false)) "the say reached its first keystroke")
      (is (= :say (:typed-step (compact/job id))))
      (let [r (compact/start! "%9" {:host latched-host :kind :compact :supersede? true})]
        (is (= :already-typed (:error r)) (pr-str (dissoc r :blocking)))
        (is (= :say (:step r)))
        (is (= id (get-in r [:blocking :job-id])))
        (is (active? id) "the typing job was left alone"))
      (finally (deliver release true)))
    (is (some? (await-job id #(= :done (:status %)))))
    (is (= ["typing"] (:sent @state)) "only the say's own text reached the pane")))

(deftest supersede-cancels-nothing-for-a-job-that-would-not-start
  (testing "say wait_idle=false on a busy pane"
    (let [{:keys [host]} (held-busy-host)
          {:keys [id run]} (start-held! host {:kind :say :text "queued"})]
      (binding [host/*host* host]
        (let [r (registry/dispatch {:command "say" :pane "%9" :text "now" :wait_idle false :supersede true})]
          (is (= :would-not-start (get-in r [:ok :error])) (pr-str r))
          (is (= :busy (get-in r [:ok :refusal :reason])))))
      (is (active? id) "the queued job keeps its slot")
      (compact/cancel! id)
      (is (= :cancelled (:status (deref run 2000 ::still-running))))))
  (testing "a compact that would end under threshold"
    (let [{:keys [host]} (held-busy-host 1000)
          {:keys [id run]} (start-held! host {:kind :say :text "queued"})
          r (compact/start! "%9" {:host host :kind :compact :supersede? true})]
      (is (= :would-not-start (:error r)))
      (is (= :under-threshold (get-in r [:refusal :reason])))
      (is (active? id))
      (compact/cancel! id)
      (is (= :cancelled (:status (deref run 2000 ::still-running))))))
  (testing "a sweep with supersede is refused before any pane is read"
    (let [{:keys [host]} (held-busy-host)
          {:keys [id run]} (start-held! host {:kind :say :text "queued"})]
      (binding [host/*host* host]
        (let [r (registry/dispatch {:command "compact" :supersede true})]
          (is (= :tmux/supersede-needs-pane (:error r)))
          (is (str/includes? (:hint r) "pane="))))
      (is (active? id))
      (compact/cancel! id)
      (is (= :cancelled (:status (deref run 2000 ::still-running)))))))

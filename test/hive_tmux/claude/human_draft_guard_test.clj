(ns hive-tmux.claude.human-draft-guard-test
  "Card 20261122160537-60f33c7a (S3): `tmux say` and `tmux compact` never
   type while a human is typing at the pane's prompt (:input non-empty) or a
   dialog is open (on screen, or the session :waiting). They keep the job
   queued (waiting, :waiting-on) or end it :skipped {:reason :human-typing |
   :open-dialog}; force=true lifts the threshold, never this guard.

   Runs through stubs of the pane-state port (the host map of `compact-test`
   with its :sessions wrapped): no tmux, no ~/.claude, no real clock, no
   with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.compact-test :as ct]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.registry :as registry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private draft "wait, I am still writing this")

(defn- pane
  "The scripted pane %9 of `ct/scripted-host`, its session status read
   through WAITING? (a fn of the pane state): while it holds, Claude Code's
   sessions/<pid>.json says \"waiting\", as it does under a permission
   prompt or a question to the human."
  ([opts] (pane opts (constantly false)))
  ([opts waiting?]
   (let [{:keys [state host]} (ct/scripted-host opts)
         sessions (:sessions host)]
     {:state state
      :host (assoc host :sessions
                   (fn [] (mapv #(cond-> % (waiting? @state) (assoc :status "waiting")) (sessions))))})))

(defn- no-keys? [state]
  (not-any? #(= "send-keys" (first %)) (:argv @state)))

;;; Pure

(deftest the-guard-names-the-human-reason
  (is (nil? (compact/human-guard {:ctx-tokens 1 :status :idle})))
  (is (= {:reason :human-typing :input "half"} (compact/human-guard {:ctx-tokens 1 :input "half"})))
  (is (= {:reason :open-dialog :status :waiting} (compact/human-guard {:ctx-tokens 1 :status :waiting})))
  (is (= :open-dialog (:reason (compact/human-guard {:dialog {:evidence "esc to cancel"} :input "x"})))
      "a dialog wins: its options, not a draft, are on the prompt line")
  (is (compact/open-dialog? {:status :waiting}))
  (is (not (compact/open-dialog? {:status :busy})))
  (is (not (compact/open-dialog? nil)))
  (is (not (compact/goal-free? {:ctx-tokens 1 :status :waiting})) "not even a /goal goes into a dialog"))

(deftest start-refusal-reads-the-guard
  (is (= :human-typing (:reason (compact/start-refusal :say {:text "hi"} {:ctx-tokens 1 :input draft}))))
  (is (= :open-dialog (:reason (compact/start-refusal :compact {:force? true} {:ctx-tokens 1 :status :waiting}))))
  (is (= :open-dialog (:reason (compact/start-refusal :say {:text "hi"} {:ctx-tokens 1 :status :waiting}))))
  (is (nil? (compact/start-refusal :compact {:force? true} {:ctx-tokens 1 :input draft}))
      "a compact waits a draft out instead of refusing at start"))

;;; An open dialog: session :waiting

(deftest say-on-a-waiting-pane-is-skipped-open-dialog
  (let [{:keys [state host]} (pane {:tokens 100000 :goal? false} (constantly true))
        job (compact/start! "%9" (assoc ct/fast :host host :kind :say :text "hello"))]
    (is (= [:skipped :open-dialog] [(:status job) (:reason job)]) (pr-str (dissoc job :steps)))
    (is (= :waiting (get-in job [:error :status])))
    (is (zero? (:clock @state)) "reported at once, not waited on as if it were work")
    (is (empty? (:sent @state)))
    (is (no-keys? state))))

(deftest compact-force-on-a-waiting-pane-is-skipped-open-dialog
  (let [{:keys [state host]} (pane {:tokens 100000 :goal? false} (constantly true))
        job (compact/start! "%9" (assoc ct/fast :host host :force? true))]
    (is (= [:skipped :open-dialog] [(:status job) (:reason job)]) (pr-str (dissoc job :steps)))
    (is (empty? (:sent @state)))
    (is (no-keys? state) "force=true does not type into a dialog")))

(deftest a-dialog-that-opens-while-a-draft-is-waited-out-stops-the-job
  ;; The draft goes out (its turn runs two polls), then the session asks the
  ;; human something: the prompt is free and the pane idle, but the job
  ;; reads the guard again on the row it would type into.
  (let [{:keys [state host]} (pane {:tokens 450000 :goal? false}
                                   (fn [s] (and (seq (:human-sent s)) (zero? (:busy-polls s)))))]
    (swap! state assoc :human draft :human-sends-in 8)
    (let [job (compact/start! "%9" (assoc ct/fast :host host))]
      (is (= [:skipped :open-dialog] [(:status job) (:reason job)]) (pr-str (dissoc job :steps)))
      (is (= :compact (get-in job [:error :during])))
      (is (= [draft] (:human-sent @state)) "the human's own text went out as theirs")
      (is (empty? (:sent @state)) "and nothing of the job's"))))

;;; A human typing: :input non-empty

(deftest force-never-types-over-a-draft
  (testing "skipped at once with input-timeout-ms 0"
    (let [{:keys [state host]} (pane {:tokens 100000 :goal? false})]
      (swap! state assoc :human draft)
      (let [job (compact/start! "%9" (assoc ct/fast :host host :force? true :input-timeout-ms 0))]
        (is (= [:skipped :human-typing] [(:status job) (:reason job)]) (pr-str (dissoc job :steps)))
        (is (= draft (get-in job [:error :input])))
        (is (no-keys? state)))))
  (testing "kept queued while the draft stands, then skipped, never typed over"
    (let [{:keys [state host]} (pane {:tokens 100000 :goal? false})
          seen (atom #{})
          host (update host :sleep! (fn [sleep!]
                                      (fn [ms]
                                        (doseq [j (compact/jobs) :when (= :running (:status j))]
                                          (some->> (:waiting-on j) (swap! seen conj)))
                                        (sleep! ms))))]
      (swap! state assoc :human draft)
      (let [job (compact/start! "%9" (assoc ct/fast :host host :force? true :input-timeout-ms 30000))]
        (is (contains? @seen {:human-typing draft :during :compact}) "while it waits it says on what")
        (is (= [:skipped :human-typing] [(:status job) (:reason job)]))
        (is (<= 30000 (:clock @state)))
        (is (no-keys? state))))))

(deftest say-is-not-queued-behind-a-draft
  (let [{:keys [state host]} (pane {:tokens 100000 :goal? false})]
    (swap! state assoc :human draft)
    (let [job (compact/start! "%9" (assoc ct/fast :host host :kind :say :text "hello" :input-timeout-ms 600000))]
      (is (= [:skipped :human-typing] [(:status job) (:reason job)]))
      (is (zero? (:clock @state)))
      (is (no-keys? state)))))

;;; The verbs

(defn- await-end [job-id]
  (loop [n 200]
    (let [j (compact/job job-id)]
      (if (or (zero? n) (not (#{:queued :running} (:status j))))
        j
        (do (Thread/sleep 5) (recur (dec n)))))))

(deftest the-verbs-carry-the-guard
  (testing "tmux compact pane=%9 force=true over a draft"
    (let [{:keys [state host]} (pane {:tokens 100000 :goal? false})]
      (swap! state assoc :human draft)
      (binding [host/*host* host]
        (let [r (registry/dispatch {:command "compact" :pane "%9" :force true :input_timeout_ms 0})
              job (await-end (get-in r [:ok :started 0 :job-id]))]
          (is (= [:skipped :human-typing] [(:status job) (:reason job)]) (pr-str r))
          (is (no-keys? state))))))
  (testing "tmux say on a waiting pane"
    (let [{:keys [state host]} (pane {:tokens 100000 :goal? false} (constantly true))]
      (binding [host/*host* host]
        (let [r (registry/dispatch {:command "say" :pane "%9" :text "hello"})
              job (await-end (get-in r [:ok :job-id]))]
          (is (= [:skipped :open-dialog] [(:status job) (:reason job)]) (pr-str r))
          (is (no-keys? state)))))))

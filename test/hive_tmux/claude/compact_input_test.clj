(ns hive-tmux.claude.compact-input-test
  "What stands at a pane's prompt: someone's draft is waited out, never typed
   over, and a slash command is read back before Enter. Runs against the
   scripted host of `compact-test`: no tmux, no ~/.claude, no real clock."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.compact-test :as ct]
            [hive-tmux.claude.composer :as composer]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private draft "half a thought about the deploy")

(defn- enter-pressed? [state]
  (some #(= "Enter" (last %)) (:argv @state)))

(deftest a-draft-is-waited-out-then-the-pane-is-compacted
  ;; Friction 20261116152435-00274559: a compact found the operator's draft at
  ;; the prompt and was skipped at once, with no retry and no notice.
  (let [{:keys [state host]} (ct/scripted-host {:tokens 450000 :goal? false})
        seen-waiting (atom #{})
        host (update host :sleep! (fn [sleep!]
                                    (fn [ms]
                                      (doseq [j (compact/jobs) :when (= :running (:status j))]
                                        (some->> (:waiting-on j) (swap! seen-waiting conj)))
                                      (sleep! ms))))]
    ;; Eight screen reads in: after the settled idle, so the /compact step
    ;; meets the draft, and before that step's wait ends.
    (swap! state assoc :human draft :human-sends-in 8)
    (let [job (compact/start! "%9" (assoc ct/fast :host host))]
      (is (= :done (:status job)) (pr-str (dissoc job :steps)))
      (is (= [draft] (:human-sent @state)) "the human's own text went out as theirs")
      (is (= ["/compact" "hive workflow catchup /caveman"] (:sent @state)))
      (is (some #(= :wait-input (:step %)) (:steps job)))
      (is (contains? @seen-waiting {:pending-human-input draft :during :compact})
          "while it waits, the job says what it waits on")
      (is (nil? (:waiting-on job)) "and says nothing once the wait is over"))))

(deftest a-turn-started-before-the-send-is-waited-out
  ;; The draft goes out between the settled idle and the /compact step: the
  ;; step finds the pane working and waits for it, instead of typing into it.
  (let [{:keys [state host]} (ct/scripted-host {:tokens 450000 :goal? false})
        seen-waiting (atom #{})
        host (update host :sleep! (fn [sleep!]
                                    (fn [ms]
                                      (doseq [j (compact/jobs) :when (= :running (:status j))]
                                        (some->> (:waiting-on j) (swap! seen-waiting conj)))
                                      (sleep! ms))))]
    (swap! state assoc :human draft :human-sends-in 4)
    (let [job (compact/start! "%9" (assoc ct/fast :host host))]
      (is (= :done (:status job)) (pr-str (dissoc job :steps)))
      (is (= [draft] (:human-sent @state)))
      (is (contains? @seen-waiting {:pane-busy true :during :compact}))
      (is (= ["/compact" "hive workflow catchup /caveman"] (:sent @state))))))

(deftest a-draft-that-stays-skips-naming-the-pane-and-the-text
  (let [{:keys [state host]} (ct/scripted-host {:tokens 450000 :goal? false})]
    (swap! state assoc :human draft)
    (let [job (compact/start! "%9" (assoc ct/fast :host host :input-timeout-ms 30000))]
      (is (= [:skipped :pending-human-input] [(:status job) (:reason job)]))
      (is (= {:during :compact :pane "%9" :name "peer one" :input draft :waited-ms 30000}
             (select-keys (:error job) [:during :pane :name :input :waited-ms])))
      (is (str/includes? (get-in job [:error :hint]) "%9"))
      (is (= draft (get-in job [:error :last :input])))
      (is (<= 30000 (:clock @state)) "it waited the whole input timeout")
      (is (empty? (:sent @state)))
      (is (not-any? #(= "send-keys" (first %)) (:argv @state)) "not one key was sent"))))

(deftest no-wait-when-the-caller-says-so
  (testing "input-timeout-ms 0 skips at once"
    (let [{:keys [state host]} (ct/scripted-host {:tokens 450000 :goal? false})]
      (swap! state assoc :human draft)
      (let [job (compact/start! "%9" (assoc ct/fast :host host :input-timeout-ms 0))]
        (is (= :pending-human-input (:reason job)))
        (is (not-any? #(= :wait-input (:step %)) (:steps job))))))
  (testing "goal with wait-idle? false does not wait on a draft"
    (let [{:keys [state host]} (ct/scripted-host {:tokens 100000 :goal? false})]
      (swap! state assoc :human draft)
      (let [job (compact/start! "%9" (assoc ct/fast :host host :kind :goal :goal "Next card." :wait-idle? false))]
        (is (= [:skipped :pending-human-input] [(:status job) (:reason job)]))
        (is (zero? (:clock @state)))))))

(deftest the-composer-must-hold-a-slash-command-before-enter
  ;; Hypothesis 20261116173922-71e59299: a paste collapsed to `[Pasted text]`
  ;; went out as a plain prompt instead of running /compact.
  (testing "pure read-back"
    (is (composer/holds? "/compact" "/compact"))
    (is (composer/holds? "/compact keep the plan" "/compact keep the\nplan") "a wrapped line")
    (is (composer/holds? "/compact" "/compact [instructions]") "a dim argument hint after it")
    (is (not (composer/holds? "/compact a\nb" "[Pasted text #1 +2 lines]")))
    (is (not (composer/holds? "/compact" "compact")) "a dropped slash")
    (is (not (composer/holds? "/compact" nil))))
  (testing "a collapsed paste is not submitted"
    (let [{:keys [state host]} (ct/scripted-host {:tokens 450000 :goal? false})]
      (swap! state assoc :collapse-paste? true)
      (let [job (compact/start! "%9" (assoc ct/fast :host host :instructions "keep:\n- the plan\n- the cards"))]
        (is (= [:failed :composer-mismatch] [(:status job) (:reason job)]))
        (is (= "[Pasted text #1 +3 lines]" (get-in job [:error :composer])))
        (is (str/starts-with? (get-in job [:error :expected]) "/compact keep:"))
        (is (not (enter-pressed? state)) "Enter was never pressed")
        (is (empty? (:sent @state))))))
  (testing "the same instructions shown whole are submitted"
    (let [{:keys [state host]} (ct/scripted-host {:tokens 450000 :goal? false})
          job (compact/start! "%9" (assoc ct/fast :host host :instructions "keep:\n- the plan"))]
      (is (= :done (:status job)) (pr-str (dissoc job :steps)))
      (is (= "/compact keep:\n- the plan" (first (:sent @state))))))
  (testing "plain text is not read back"
    (let [{:keys [state host]} (ct/scripted-host {:tokens 100000 :goal? false})]
      (swap! state assoc :collapse-paste? true)
      (let [job (compact/start! "%9" (assoc ct/fast :host host :kind :say :text "line one\nline two"))]
        (is (= :done (:status job)))
        (is (= ["[Pasted text #1 +2 lines]"] (:sent @state)) "a collapsed prompt is still that prompt")))))

(ns hive-tmux.claude.caller-test
  "Card 20261122175413-3d66aa77: since bb-mcp #45 the caller id is
   coordinator:<first 8 hex of the session id>. The pid-only reader returned
   nil for that id, so every own-pane guard failed OPEN. These tests show that
   a hex-id caller cannot compact its own pane (named or in a sweep), cannot
   answer its own dialog, and that S2's auto-compact refuses its own pane;
   and that a coordinator id naming nothing is refused (fail closed).

   Every sensor is a stub port: the host map (one pane %4 whose session is
   6fe2d4c2-…, pid 4242), the watcher's panes-fn, lease, evidence and
   start-job!. No tmux, no ~/.claude, no with-redefs."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.caller :as caller]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.registry :as registry]
            [hive-tmux.claude.watch :as watch]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private sid "6fe2d4c2-b58f-46f9-9067-772eb1a4a04d")
(def ^:private me-hex "coordinator:6fe2d4c2")
(def ^:private me-pid "coordinator:4242")

;;; Promote

(deftest caller-token-reads-both-id-forms
  (is (= {:session-token "6fe2d4c2"} (caller/caller-token me-hex)))
  (is (= {:pid 4242} (caller/caller-token me-pid)))
  (is (= {:session-token "00123456"} (caller/caller-token "coordinator:00123456"))
      "8 digits is a session token, never pid 123456")
  (doseq [bad ["coordinator:" "coordinator:0" "coordinator:6FE2D4C2" "coordinator:6fe2d4c" "coordinator:zz"
               "coordinator:6fe2d4c2-b58f" "ling-7:4242" nil]]
    (is (nil? (caller/caller-token bad)) (pr-str bad))))

(deftest identity-fails-closed-on-a-coordinator-id-that-names-nothing
  (is (= {:token {:session-token "6fe2d4c2"}} (caller/identity-of me-hex)))
  (is (= {:token {:pid 4242}} (caller/identity-of me-pid)))
  (is (= {:unresolvable "coordinator:zz"} (caller/identity-of "coordinator:zz")))
  (is (nil? (caller/identity-of "ling-7:4242")) "not a coordinator: no pane is its own")
  (is (nil? (caller/identity-of nil))))

(deftest own-pane-matches-pid-or-session-prefix
  (let [row {:pane "%4" :pid 4242 :session-id sid}]
    (is (caller/own-pane? row {:session-token "6fe2d4c2"}))
    (is (caller/own-pane? row {:pid 4242}))
    (is (caller/own-pane? row 4242) "a bare pid, as the old caller-pid gave")
    (is (not (caller/own-pane? row {:session-token "deadbeef"})))
    (is (not (caller/own-pane? row {:pid 1})))
    (is (not (caller/own-pane? (dissoc row :session-id) {:session-token "6fe2d4c2"})))
    (is (not (caller/own-pane? row nil)))))

;;; Boundary: the verbs, through a stub host

(defn- stub-host
  "One Claude pane %4, the session of pid 4242 / SID, at 450k tokens, idle,
   with a menu open when MENU?. Records every tmux argv under :argv."
  [{:keys [menu?]}]
  (let [st (atom {:argv []})]
    {:state st
     :host
     {:tmux! (fn tmux!
               ([argv] (tmux! argv nil))
               ([argv _]
                (swap! st update :argv conj argv)
                (case (first argv)
                  "list-panes" {:exit 0 :err "" :out "%4\t1\t1\t1\t4242\tclaude\t/srv/me\t✳ me\t/dev/pts/4\n"}
                  "capture-pane" {:exit 0 :err ""
                                  :out (str/join "\n"
                                                 (if menu?
                                                   ["● Pick one" "❯ 1. Yes" "  2. No" "Enter to select · ↑/↓ to navigate · Esc to cancel"]
                                                   ["✻ Worked for 2s" "   450000 tokens" "──── me ─" "❯\u00a0" "────" "  -- INSERT --"]))}
                  {:exit 0 :out "" :err ""})))
      :sessions (fn [] [{:pid 4242 :sessionId sid :name "me" :cwd "/srv/me" :status "idle" :updatedAt 1}])
      :transcript (fn [_ _] [])
      :sleep! (fn [_])
      :now-ms (fn [] 0)}}))

(defn- keys-sent? [state] (some #(= "send-keys" (first %)) (:argv @state)))

(deftest a-hex-id-caller-cannot-compact-its-own-pane
  (let [{:keys [state host]} (stub-host {})
        jobs-before (count (compact/jobs))]
    (binding [host/*host* host]
      (testing "named"
        (let [r (registry/dispatch {:command "compact" :pane "%4" :_caller_id me-hex})]
          (is (= :tmux/own-pane (:error r)) (pr-str r))))
      (testing "in a sweep it is left out"
        (let [{:keys [ok] :as r} (registry/dispatch {:command "compact" :_caller_id me-hex})]
          (is (empty? (:started ok)) (pr-str r))
          (is (= ["%4"] (mapv :pane (get-in ok [:skipped-own-pane :own-pane]))))))
      (testing "the pid form still works"
        (is (= :tmux/own-pane (:error (registry/dispatch {:command "compact" :pane "%4" :_caller_id me-pid})))))
      (testing "another session's pane is compacted as before"
        (let [r (registry/dispatch {:command "compact" :pane "%4" :dry_run true :_caller_id "coordinator:deadbeef"})]
          (is (= ["%4"] (mapv :pane (get-in r [:ok :would-compact]))) (pr-str r)))))
    (is (= jobs-before (count (compact/jobs))) "no job was started")
    (is (not (keys-sent? state)) "not one key was sent")))

(deftest an-unresolvable-coordinator-id-is-refused-not-trusted
  (let [{:keys [state host]} (stub-host {})]
    (binding [host/*host* host]
      (doseq [params [{:command "compact" :pane "%4"} {:command "compact"} {:command "compact" :dry_run true}]]
        (let [r (registry/dispatch (assoc params :_caller_id "coordinator:not-a-token"))]
          (is (= :tmux/unresolvable-caller (:error r)) (pr-str params r))
          (is (string? (:why r))))))
    (is (not (keys-sent? state)))))

(deftest a-hex-id-caller-cannot-answer-its-own-dialog
  (let [{:keys [state host]} (stub-host {:menu? true})]
    (binding [host/*host* host]
      (let [r (registry/dispatch {:command "answer" :pane "%4" :option "1" :_caller_id me-hex})]
        (is (= :tmux/own-pane (:error r)) (pr-str r)))
      (let [r (registry/dispatch {:command "answer" :pane "%4" :option "1" :_caller_id "coordinator:not-a-token"})]
        (is (= :tmux/unresolvable-caller (:error r)) (pr-str r))))
    (is (not (keys-sent? state)) "no key reached the dialog")))

;;; S2: the auto-compact path

(defn- run-auto-compact [parent row]
  (let [rows (atom [(assoc row :ctx-tokens 300000)])
        started (atom [])]
    (watch/start! {:parent parent :interval-ms 3600000 :auto-compact? true :idle-capacity? false
                   :host {:tmux! (fn [& _] {:exit 1 :out "" :err ""}) :sessions (fn [] [])
                          :transcript (fn [_ _] nil) :sleep! (fn [_])
                          :now-ms (let [t (atom 0)] #(swap! t + 1000))}
                   :panes-fn (fn [_] @rows)
                   :shout-fn (fn [& _] true)
                   :evidence-fn (fn [_] {:wrapped-at 10000000})
                   :silencer (fn [_] {:ok true})
                   :lease (let [a (atom nil)] {:read #(deref a) :write! #(reset! a %)})
                   :start-job! (fn [pane _] (swap! started conj pane) {:job-id "cc-auto"})})
    (try
      (watch/tick! (#'watch/current))
      (reset! rows [row])
      (watch/tick! (#'watch/current))
      {:started @started :auto (:auto-compacts (watch/status))}
      (finally (watch/stop!)))))

(deftest auto-compact-refuses-a-hex-id-compactors-own-pane
  (let [row {:pane "%4" :target "1:1.1" :name "me" :session-id sid :pid 4242 :ctx-tokens 420000
             :busy? false :status :idle :provider :claude}]
    (testing "hex id"
      (let [{:keys [started auto]} (run-auto-compact me-hex row)]
        (is (empty? started))
        (is (= :own-pane (:refused (last auto))) (pr-str auto))))
    (testing "the own pane is refused even when the row's pid is unknown"
      (let [{:keys [started auto]} (run-auto-compact me-hex (dissoc row :pid))]
        (is (empty? started))
        (is (= :own-pane (:refused (last auto))))))
    (testing "another session's pane is compacted"
      (let [{:keys [started auto]} (run-auto-compact "coordinator:deadbeef" row)]
        (is (= #{"%4"} (set started)) (pr-str auto))
        (is (not-any? :refused auto))))))

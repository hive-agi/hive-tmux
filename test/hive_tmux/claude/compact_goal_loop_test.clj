(ns hive-tmux.claude.compact-goal-loop-test
  "compact! on a pane its /goal Stop hook keeps re-prompting (card
   20261122170735-7c6f5ad3, scope 2), through the scripted host port
   `stop-hook-pane` (Claude Code 2.1.330 as measured). Each deftest names the
   friction whose measured repro it replays:

     2256a137  pane %8 never idle; `compact force interrupt` waited forever and
               a typed /compact sat Queued behind `running Stop hooks 1/2`
     220d00ce  a force-sent /compact left Queued; the job timed out and dropped
               its reseed and goal restore
     500f60d0  after /compact the goal hook resumed the session before the
               reseed went out; the reseed was half-typed
     387a0b74  :done :pane-busy-after-compact, :reseed-pending, nothing typed

   No tmux, no ~/.claude, no clock, no with-redefs: the host is a port map."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.ready :as ready]
            [hive-tmux.claude.stop-hook-pane :as shp]
            [hive-tmux.claude.watch :as watch]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private fast {:poll-ms 1000 :settle-ms 2000 :sync? true})

(def ^:private goal-cmd (str "/goal " shp/goal-text))

(defn- reseed-of [sent] (some #(when (str/starts-with? % "hive workflow catchup") %) sent))

(deftest f2256a137-a-goal-looping-pane-is-compacted-and-its-goal-survives
  (let [{:keys [state host]} (shp/stop-hook-pane)
        job (compact/start! "%8" (assoc fast :host host :force? true :interrupt true :idle-timeout-ms 600000))
        sent (:sent @state)]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (true? (:goal-loop? job)))
    (is (= ["/goal clear" "/compact"] (take 2 sent)) "clear the looping goal, then compact")
    (is (str/includes? (reseed-of sent) shp/goal-text) "the reseed carries the goal text")
    (is (= goal-cmd (last sent)) "and the goal is set again, the same text")
    (is (true? (:started? job)) "the restored goal showed on the footer")
    (is (= 30000 (:tokens-after-compact job)))
    (is (zero? (:typed-while-busy @state)) "nothing but /goal was typed into a working turn")
    (is (< (:clock @state) 60000) "minutes, not forever")))

(deftest f220d00ce-a-compact-already-queued-runs-at-the-clear-and-is-not-typed-twice
  ;; An earlier attempt left /compact Queued on the looping pane.
  (let [{:keys [state host]} (shp/stop-hook-pane {:queued ["/compact"]})
        job (compact/start! "%8" (assoc fast :host host :force? true :idle-timeout-ms 600000))
        sent (:sent @state)]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (= 1 (count (filter #(str/starts-with? % "/compact") sent))) (pr-str sent))
    (is (some :ran-at-goal-clear (:steps job)))
    (is (str/includes? (reseed-of sent) shp/goal-text))
    (is (= goal-cmd (last sent)) "the reseed and the goal restore are NOT dropped")))

(deftest f500f60d0-the-reseed-goes-out-whole-before-the-goal-loop-resumes
  (let [{:keys [state host]} (shp/stop-hook-pane)
        job (compact/start! "%8" (assoc fast :host host :force? true))
        sent (:sent @state)
        reseed (reseed-of sent)]
    (is (= :done (:status job)))
    (is (< (.indexOf sent reseed) (.indexOf sent goal-cmd))
        "the goal (and its Stop hook) comes back only after the reseed was submitted")
    (is (= (compact/goal-reseed compact/default-reseed shp/goal-text) reseed) "submitted whole")
    (is (nil? (:typed @state)) "nothing is left half-typed in the composer")))

(deftest f387a0b74-no-reseed-is-silently-dropped
  (let [{:keys [host]} (shp/stop-hook-pane)
        job (compact/start! "%8" (assoc fast :host host :force? true))]
    (is (not= :pane-busy-after-compact (:reason job)))
    (is (nil? (:reseed-pending job)))
    (is (empty? (:unsent job)))))

(deftest a-human-draft-is-never-typed-over
  ;; The S3 human-draft guard holds on the goal-loop path too: the clear waits
  ;; the draft out, then gives up :skipped :human-typing, nothing typed.
  (let [{:keys [state host]} (shp/stop-hook-pane {:human "wait, I am still writing this"})
        job (compact/start! "%8" (assoc fast :host host :force? true :input-timeout-ms 20000))]
    (is (= [:skipped :human-typing] [(:status job) (:reason job)]) (pr-str (dissoc job :steps)))
    (is (empty? (:sent @state)))
    (is (zero? (:typed-while-busy @state)))))

(deftest an-unreadable-goal-is-not-cleared
  (let [{:keys [state host]} (shp/stop-hook-pane {:transcript []})
        job (compact/start! "%8" (assoc fast :host host :force? true))]
    (is (= [:skipped :goal-unreadable] [(:status job) (:reason job)]))
    (is (empty? (:sent @state)) "a goal that cannot be restored is never cleared"))
  (testing "a goal= override makes it restorable"
    (let [{:keys [state host]} (shp/stop-hook-pane {:transcript []})
          job (compact/start! "%8" (assoc fast :host host :force? true :goal "Lane goal from the caller."))]
      (is (= :done (:status job)) (pr-str (dissoc job :steps)))
      (is (= "/goal Lane goal from the caller." (last (:sent @state)))))))

(deftest goal-loop-false-keeps-the-old-waits
  (let [{:keys [state host]} (shp/stop-hook-pane)
        job (compact/start! "%8" (assoc fast :host host :force? true :goal-loop false :interrupt false
                                        :idle-timeout-ms 20000))]
    (is (= [:failed :timeout] [(:status job) (:reason job)]))
    (is (empty? (:sent @state)))))

(deftest goal-reseed-text
  (is (= "r -- then resume your /goal, paused for this compaction and set again right after: g"
         (compact/goal-reseed "r" "g")))
  (is (= "r" (compact/goal-reseed "r" nil)))
  (is (nil? (compact/goal-reseed "" "g")) "no reseed asked for: none is invented"))

;;; S2: the goal-looping pane reaches compact through the ONE compactor

(def ^:private sid "6fe2d4c2-aaaa")

(defn- loop-row [m]
  (merge {:pane "%8" :target "1:3.1" :name "coordinator one" :session-id sid :pid 100
          :ctx-tokens 470000 :busy? true :status :busy :goal-active? true :stop-hook-running? true} m))

(deftest a-goal-looping-pane-is-compact-ready-once-its-coordinator-wrapped
  (is (= {:ready? true :evidence {:by :wrapped :at 2000}}
         (ready/verdict (loop-row {}) 400000 1000 {:wrapped-at 2000})))
  (is (some #{:busy} (:blockers (ready/verdict (loop-row {:stop-hook-running? false}) 400000 1000 {:wrapped-at 2000})))
      "a /goal pane busy in a long tool call, with no loop evidence, is still busy")
  (is (some #{:human-typing} (:blockers (ready/verdict (loop-row {:input "draft"}) 400000 1000 {:wrapped-at 2000})))))

(deftest auto-compact-routes-a-goal-looping-pane-through-the-lease
  (let [rows (atom [(loop-row {:ctx-tokens 300000})])
        started (atom [])
        lease (atom nil)
        st (watch/start! {:parent "coordinator:72216f39" :interval-ms 3600000 :auto-compact? true
                          :host {:tmux! (fn [& _] {:exit 1 :out "" :err ""}) :sessions (fn [] [])
                                 :transcript (fn [_ _] nil) :sleep! (fn [_])
                                 :now-ms (let [t (atom 0)] #(swap! t + 1000))}
                          :panes-fn (fn [_] @rows) :idle-capacity? false
                          :shout-fn (fn [& _] true)
                          :evidence-fn (fn [_] {:wrapped-at 10000000})
                          :silencer (fn [_] {:ok true})
                          :lease {:read #(deref lease) :write! #(reset! lease %)}
                          :start-job! (fn [pane opts] (swap! started conj [pane opts]) {:job-id "cc-auto"})})]
    (try
      (is (:running? st))
      (watch/tick! (#'watch/current))
      (reset! rows [(loop-row {})])
      (watch/tick! (#'watch/current))
      (is (= 1 (count @started)) "the looping pane was compacted, by the lease holder")
      (is (= "%8" (ffirst @started)))
      (is (= "coordinator:72216f39" (:owner @lease)))
      (finally (watch/stop!)))))

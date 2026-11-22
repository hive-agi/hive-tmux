(ns hive-tmux.claude.compact-ready-test
  "Card 20261122160542-14c8ef6c (S2): the watcher emits compact-ready only
   when ALL hold (ctx >= threshold, idle, empty :input, not :waiting, and a
   WRAPPED shout or a handoff memory updated after the crossing), always
   matched by session id; the optional auto-compact is owned by exactly ONE
   compactor (decision 20261121215259-7b40b779), silences a wrapped
   coordinator's wakers, and re-seeds with its hand-off.

   Every port is a stub map (pane-state host, evidence, lease, silencer,
   start-job!): no tmux, no ~/.claude, no host memory, no with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.ready :as ready]
            [hive-tmux.claude.sense :as sense]
            [hive-tmux.claude.watch :as watch]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def sid "6fe2d4c2-b58f-46f9-9067-772eb1a4a04d")

(defn- row [m]
  (merge {:pane "%8" :target "2:3.1" :name "coord" :session-id sid :pid 1098705
          :ctx-tokens 420000 :busy? false :status :idle :provider :claude} m))

(def wrapped {:wrapped-at 2000})
(def handoff {:handoff {:id "20261122-handoff" :updated-ms 2000}})

;;; Promote: the verdict

(deftest ready-only-when-every-condition-holds
  (is (= {:ready? true :evidence {:by :wrapped :at 2000}}
         (ready/verdict (row {}) 400000 1000 wrapped)))
  (is (= {:ready? true :evidence {:by :handoff :at 2000 :handoff-id "20261122-handoff"}}
         (ready/verdict (row {}) 400000 1000 (merge wrapped handoff)))
      "a hand-off names what to read, so it wins over a bare WRAPPED")
  (doseq [[why r ev] [[:under-threshold (row {:ctx-tokens 100000}) wrapped]
                      [:busy (row {:busy? true :status :busy}) wrapped]
                      [:human-typing (row {:input "half a prompt"}) wrapped]
                      [:open-dialog (row {:status :waiting}) wrapped]
                      [:open-dialog (row {:dialog {:evidence "esc to cancel"}}) wrapped]
                      [:no-session (row {:session-id nil}) wrapped]
                      [:no-wrap-or-handoff (row {}) nil]]]
    (let [v (ready/verdict r 400000 1000 ev)]
      (is (false? (:ready? v)) (pr-str why))
      (is (some #{why} (:blockers v)) (pr-str why v)))))

(deftest evidence-must-come-after-the-crossing
  (is (nil? (ready/evidence-after {:wrapped-at 900} 1000)) "a WRAPPED from before the crossing is an older wrap")
  (is (nil? (ready/evidence-after {:handoff {:id "h" :updated-ms 1000}} 1000)) "strictly after")
  (is (= :wrapped (:by (ready/evidence-after {:wrapped-at 1001 :handoff {:id "h" :updated-ms 10}} 1000)))
      "an old hand-off does not mask a fresh WRAPPED"))

(deftest a-coordinator-is-named-by-its-session-never-its-pane
  (is (= ["6fe2d4c2" "1098705"] (ready/coordinator-tokens (row {}))))
  (is (= ["coordinator:6fe2d4c2" "coordinator:1098705"] (ready/agent-ids (row {}))))
  (is (ready/own-row? (row {}) "coordinator:6fe2d4c2"))
  (is (ready/own-row? (row {}) "coordinator:1098705"))
  (is (not (ready/own-row? (row {:session-id "deadbeef-0000" :pid 7}) "coordinator:6fe2d4c2"))))

(deftest the-reseed-names-the-hand-off
  (is (= "hive workflow catchup /caveman, then read hand-off 20261122-handoff and resume"
         (ready/reseed-text {:handoff-id "20261122-handoff"})))
  (is (re-find #"^hive workflow catchup /caveman, then read hand-off .+ and resume$" (ready/reseed-text {:by :wrapped}))))

(deftest one-compactor-lease
  (is (:ok? (ready/lease-verdict nil "coordinator:a" 0 false)))
  (is (:ok? (ready/lease-verdict {:owner "coordinator:a" :renewed-ms 0} "coordinator:a" 10 false)) "the owner renews")
  (is (= {:ok? false :holder "coordinator:a" :renewed-ms 0}
         (ready/lease-verdict {:owner "coordinator:a" :renewed-ms 0} "coordinator:b" 10 false))
      "a second compactor is refused while the lease is fresh")
  (is (:ok? (ready/lease-verdict {:owner "coordinator:a" :renewed-ms 0} "coordinator:b" (inc ready/lease-ttl-ms) false))
      "a lease nobody renewed is free")
  (is (:ok? (ready/lease-verdict {:owner "coordinator:a" :renewed-ms 0} "coordinator:b" 10 true)) "takeover"))

(deftest host-evidence-readers
  (is (= 3000 (ready/wrapped-at [{:message "WRAPPED: hand-off 1" :timestamp 3000}
                                 {:message "progress" :timestamp 5000}
                                 {:m "WRAPPED" :timestamp "1970-01-01T00:00:01Z"}])))
  (is (nil? (ready/wrapped-at [{:message "still working" :timestamp 1}])))
  (is (= {:id "b" :updated-ms 86400000}
         (ready/newest-handoff [{:id "a" :updated "1970-01-01T00:00:01Z"}
                                {:id "b" :updated "1970-01-02T00:00:00-00:00[UTC]"}]))))

;;; Promote: the watcher's step

(defn- run
  "Feed [rows evidence now] ticks through `watch/step`; the compact-ready
   events as [pane session-id evidence-by]."
  [ticks]
  (loop [states {} [[rows ev now] & more] ticks out []]
    (if-not rows
      out
      (let [{s :states evs :events} (watch/step states rows {:threshold 400000 :now now :evidence ev})]
        (recur s more (into out (keep (fn [[k r e]] (when (= :compact-ready k) [(:pane r) (:session-id r) (:by e)])) evs)))))))

(deftest compact-ready-fires-once-per-spell-after-the-wrap
  (is (= [["%8" sid :wrapped]]
         (run [[[(row {:ctx-tokens 300000})] {} 0]
               [[(row {})] {} 1000]                        ; crosses at 1000
               [[(row {})] {"%8" {:wrapped-at 900}} 1500]  ; an older wrap: not yet
               [[(row {})] {"%8" {:wrapped-at 2000}} 2500] ; wrapped after the crossing
               [[(row {})] {"%8" {:wrapped-at 2000}} 3000]]))) ; once
  (testing "a busy, typing or waiting pane holds it back until it is free"
    (is (= [["%8" sid :wrapped]]
           (run [[[(row {:ctx-tokens 300000})] {} 0]
                 [[(row {:busy? true :status :busy})] {"%8" wrapped} 1000]
                 [[(row {:input "draft"})] {"%8" {:wrapped-at 3000}} 2000]
                 [[(row {:status :waiting})] {"%8" {:wrapped-at 3000}} 3000]
                 [[(row {})] {"%8" {:wrapped-at 3000}} 4000]]))))
  (testing "dropping under the threshold (a compaction) re-arms it"
    (is (= [["%8" sid :wrapped] ["%8" sid :handoff]]
           (run [[[(row {})] {"%8" {:wrapped-at 10}} 1]
                 [[(row {:ctx-tokens 40000})] {} 2]
                 [[(row {})] {} 3]
                 [[(row {})] {"%8" {:handoff {:id "h2" :updated-ms 4}}} 5]])))))

(deftest a-reused-pane-id-is-a-new-session
  ;; %8 fired for the old session; a new session then takes %8 over the threshold.
  (let [other "0a0a0a0a-1111-2222-3333-444444444444"]
    (is (= [["%8" sid :wrapped] ["%8" other :wrapped]]
           (run [[[(row {})] {"%8" {:wrapped-at 10}} 1]
                 [[(row {:session-id other :pid 2})] {"%8" {:wrapped-at 10}} 20] ; the old session's wrap is older
                 [[(row {:session-id other :pid 2})] {"%8" {:wrapped-at 30}} 40]]))
        "the new session gets its own crossing, and the old wrap does not count for it")))

(deftest evidence-is-read-only-for-free-panes
  (let [rows [(row {}) (row {:pane "%9" :busy? true :status :busy :session-id "x"})
              (row {:pane "%10" :input "typing" :session-id "y"}) (row {:pane "%11" :ctx-tokens 1 :session-id "z"})]]
    (is (= ["%8"] (map :pane (watch/evidence-wanted {} rows {:threshold 400000}))))
    (testing "under auto-compact a busy high pane is read too: its wakers may be what keeps it busy"
      (is (= ["%8" "%9" "%10"] (map :pane (watch/evidence-wanted {} rows {:threshold 400000 :auto-compact? true})))))
    (testing "excluded and already-fired panes are not read"
      (is (empty? (watch/evidence-wanted {"%8" {:ready-fired? true :session-id sid}} [(row {})] {:threshold 400000})))
      (is (empty? (watch/evidence-wanted {} [(row {})] {:threshold 400000 :exclude ["%8"]}))))))

(deftest the-sense-class-and-fields
  (is (= :sense/compact-ready (sense/event->class :compact-ready)))
  (is (= "handoff_id" (sense/detail-keys :tmux-handoff-id)))
  (is (= {:tmux-pane "%8" :tmux-session-id sid :tmux-ready-by "handoff" :tmux-handoff-id "h"
          :tmux-coordinator "coordinator:6fe2d4c2"}
         (select-keys (watch/ready-data (row {}) {:by :handoff :handoff-id "h"})
                      [:tmux-pane :tmux-session-id :tmux-ready-by :tmux-handoff-id :tmux-coordinator]))))

;;; Boundary: the running watcher, through stub ports

(defn- pane-host
  "A pane-state host stub: `rows` (an atom of pane rows) is what `panes/panes`
   reads. It is the panes port seen through the watcher's :panes-fn seam."
  [rows]
  {:tmux! (fn [& _] {:exit 1 :out "" :err "stub"}) :sessions (fn [] []) :transcript (fn [_ _] nil)
   :sleep! (fn [_]) :now-ms (let [t (atom 0)] #(swap! t + 1000)) :rows rows})

(defn- mem-lease [init] (let [a (atom init)] {:read #(deref a) :write! #(reset! a %) :atom a}))

(defn- start [rows & {:as o}]
  (let [shouts (atom []) started (atom []) silenced (atom [])
        lease (or (:lease o) (mem-lease nil))
        st (watch/start! (merge {:parent "coordinator:72216f39" :host (pane-host rows) :interval-ms 3600000
                                 :panes-fn (fn [_] @rows)
                                 :idle-capacity? false ; stub ports only: no host pulse or board
                                 :shout-fn (fn [a e d] (swap! shouts conj [a e d]) true)
                                 :evidence-fn (fn [r] (when (= sid (:session-id r)) {:wrapped-at 10000000}))
                                 :silencer (fn [r] (swap! silenced conj (:session-id r)) {:ok true})
                                 :lease lease
                                 :start-job! (fn [pane opts] (swap! started conj [pane opts]) {:job-id "cc-auto" :pane pane})}
                                (dissoc o :lease)))]
    {:st st :shouts shouts :started started :silenced silenced :lease lease}))

(deftest auto-compact-is-owned-by-one-compactor
  (let [lease (mem-lease {:owner "coordinator:593a2651" :renewed-ms 1000})
        {:keys [st started]} (start (atom [(row {})]) :auto-compact? true :lease lease)]
    (is (= :lease-held (:refused st)) (pr-str st))
    (is (= "coordinator:593a2651" (:holder st)))
    (is (re-find #"7b40b779" (:hint st)))
    (is (false? (watch/running?)) "refused before starting")
    (is (empty? @started))
    (is (= "coordinator:593a2651" (:owner @(:atom lease))) "the holder's lease is untouched")))

(deftest auto-compact-silences-the-wrapped-coordinator-then-compacts-the-same-session
  (let [rows (atom [(row {:ctx-tokens 300000})])
        {:keys [shouts started silenced lease]} (start rows :auto-compact? true)]
    (try
      (is (= "coordinator:72216f39" (:owner @(:atom lease))) "the watcher holds the lease")
      (watch/tick! (#'watch/current))                               ; seed under the threshold
      (reset! rows [(row {:busy? true :status :busy})])            ; crosses; its wakers keep it busy
      (watch/tick! (#'watch/current))
      (is (= [sid] @silenced) "a WRAPPED coordinator's wakers are silenced, once")
      (is (empty? @started) "busy: not compacted")
      (reset! rows [(row {})])                                     ; now it idles
      (watch/tick! (#'watch/current))
      (watch/tick! (#'watch/current))
      (is (= [sid] @silenced) "still once")
      (is (= 1 (count @started)) (pr-str @started))
      (let [[pane opts] (first @started)]
        (is (= "%8" pane))
        (is (re-find #"then read hand-off .+ and resume" (:reseed opts))))
      (is (= 1 (count (filter #(= :compact-ready (second %)) @shouts))))
      (let [[_ _ d] (first (filter #(= :compact-ready (second %)) @shouts))]
        (is (= "coordinator:72216f39" (:parent-id d)))
        (is (= sid (:tmux-session-id d))))
      (finally (watch/stop!)))
    (is (nil? @(:atom lease)) "stop releases the lease")))

(deftest auto-compact-refuses-a-pane-that-changed-session-and-its-own-pane
  (testing "the pane is re-read before the job: a reused pane id is refused"
    (let [rows (atom [(row {:ctx-tokens 300000})])
          flip (atom false)
          {:keys [started]} (start rows :auto-compact? true
                                   :panes-fn (fn [_] (if @flip
                                                       [(row {:session-id "ffffffff-new" :pid 3})]
                                                       (do (when (< 300000 (:ctx-tokens (first @rows))) (reset! flip true))
                                                           @rows))))]
      (try
        (watch/tick! (#'watch/current))
        (reset! rows [(row {})])
        (watch/tick! (#'watch/current))
        (is (empty? @started))
        (is (= :session-changed (:refused (last (:auto-compacts (watch/status))))) (pr-str (watch/status)))
        (finally (watch/stop!)))))
  (testing "the compactor never compacts its own pane"
    (let [rows (atom [(row {:ctx-tokens 300000})])
          {:keys [started]} (start rows :auto-compact? true :parent "coordinator:6fe2d4c2")]
      (try
        (watch/tick! (#'watch/current))
        (reset! rows [(row {})])
        (watch/tick! (#'watch/current))
        (is (empty? @started))
        (is (= :own-pane (:refused (last (:auto-compacts (watch/status))))))
        (finally (watch/stop!))))))

(deftest without-auto-compact-the-sense-only-reports
  (let [rows (atom [(row {:ctx-tokens 300000})])
        {:keys [shouts started silenced lease]} (start rows)]
    (try
      (watch/tick! (#'watch/current))
      (reset! rows [(row {})])
      (watch/tick! (#'watch/current))
      (is (= 1 (count (filter #(= :compact-ready (second %)) @shouts))))
      (is (empty? @started))
      (is (empty? @silenced))
      (is (nil? @(:atom lease)) "no lease is taken")
      (finally (watch/stop!)))))

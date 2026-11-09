(ns hive-tmux.claude.watch-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.sense :as sense]
            [hive-tmux.claude.watch :as watch]))

(def opts {:threshold 400000})

(defn- row [m] (merge {:pane "%1" :target "1:1.1" :name "peer" :ctx-tokens 100000
                       :busy? false :goal-active? true :goal-ended? false} m))

(defn- run-ticks
  "Feed ROWS-PER-TICK (each a vector of rows) through step; every event kind
   in order, as [kind pane]."
  [rows-per-tick & {:keys [o] :or {o opts}}]
  (loop [states {} [rows & more] rows-per-tick events []]
    (if-not rows
      events
      (let [{s :states evs :events} (watch/step states rows o)]
        (recur s more (into events (map (fn [[k r]] [k (:pane r)]) evs)))))))

(deftest the-first-tick-only-seeds
  (is (empty? (run-ticks [[(row {:ctx-tokens 900000 :goal-active? false})]]))
      "a pane already idle, already high, already goal-less when the watch starts is not news"))

(deftest a-pane-that-worked-is-idle-once-on-the-second-idle-tick
  (is (= [[:pane-idle "%1"]]
         (run-ticks [[(row {})] [(row {:busy? true})] [(row {})] [(row {})] [(row {})] [(row {})]])))
  (testing "a pane never seen working never yields idle"
    (is (empty? (run-ticks [[(row {})] [(row {})] [(row {})]]))))
  (testing "a new working spell yields a new idle"
    (is (= [[:pane-idle "%1"] [:pane-idle "%1"]]
           (run-ticks [[(row {})] [(row {:busy? true})] [(row {})] [(row {})]
                       [(row {:busy? true})] [(row {})] [(row {})]])))))

(deftest goal-ended-fires-on-the-active-to-inactive-edge
  (is (= [[:goal-ended "%1"]]
         (run-ticks [[(row {})] [(row {:goal-active? false})] [(row {:goal-active? false})]])))
  (testing "an unreadable screen (no token count) is not a goal ending"
    (is (empty? (run-ticks [[(row {})] [(row {:goal-active? false :ctx-tokens nil})]]))))
  (testing "the Goal achieved line counts too"
    (is (= [[:goal-ended "%1"]]
           (run-ticks [[(row {:goal-active? false})] [(row {:goal-active? false :goal-ended? true})]])))))

(deftest context-high-fires-when-crossing-up-and-rearms-below
  (is (= [[:context-high "%1"] [:context-high "%1"]]
         (run-ticks [[(row {:ctx-tokens 390000})] [(row {:ctx-tokens 401000})] [(row {:ctx-tokens 420000})]
                     [(row {:ctx-tokens 40000})] [(row {:ctx-tokens 405000})]]))))

(deftest pane-waiting-fires-once-per-waiting-spell
  (is (= [[:pane-waiting "%1"]]
         (run-ticks [[(row {:status :busy})] [(row {:status :waiting})]]))
      "the edge into :waiting fires")
  (is (= [[:pane-waiting "%1"]]
         (run-ticks [[(row {:status :busy})] [(row {:status :waiting})]
                     [(row {:status :waiting})] [(row {:status :waiting})]]))
      "staying :waiting does not fire again")
  (is (empty? (run-ticks [[(row {:status :waiting})] [(row {:status :waiting})]]))
      "the seed tick fires nothing, and a pane already waiting then is not news")
  (testing "a new waiting spell fires again"
    (is (= [[:pane-waiting "%1"] [:pane-waiting "%1"]]
           (run-ticks [[(row {:status :busy})] [(row {:status :waiting})]
                       [(row {:status :busy})] [(row {:status :waiting})]]))))
  (testing "the shout data carries the status"
    (is (= "waiting" (:tmux-status (watch/row-data (row {:status :waiting})))))
    (is (= "pane_status" (sense/detail-keys :tmux-status)))))

(deftest excluded-panes-are-never-tracked
  (is (empty? (run-ticks [[(row {})] [(row {:busy? true})] [(row {})] [(row {})]]
                         :o (assoc opts :exclude ["peer"])))))

(deftest every-event-has-a-sense-class
  (is (= #{:pane-idle :goal-ended :context-high :pane-waiting :tmux-job} (set (keys sense/event->class))))
  (testing "a class perceives exactly its own event type"
    (let [specs (sense/class-specs)
          by (into {} (map (juxt :sense/class identity) specs))]
      (is ((:perceives? (by :sense/pane-idle)) {:event-type :pane-idle}))
      (is (not ((:perceives? (by :sense/pane-idle)) {:event-type :goal-ended}))))))

(deftest a-running-watcher-shouts-to-its-parent
  ;; The pane works for its first 3 captures, then idles; the test waits on the
  ;; idle shout itself (a promise), not on a clock.
  (let [shouts (atom [])
        heard-idle (promise)
        captures (atom 0)
        host {:tmux! (fn tmux!
                       ([argv] (tmux! argv nil))
                       ([argv _]
                        (case (first argv)
                          "list-panes" {:exit 0 :out "%7\t1\t7\t1\t9\tclaude\t/srv/x\t✳ peer seven\n" :err ""}
                          "capture-pane" {:exit 0 :err ""
                                          :out (str (if (<= (swap! captures inc) 3) "✶ Working… (1s)\n" "✻ Worked for 1s\n")
                                                    "   120000 tokens\n──── peer seven ─\n❯ \n")})))
              :sessions (fn [] []) :transcript (fn [_ _] nil)
              :sleep! (fn [_]) :now-ms (fn [] 0)}]
    (watch/start! {:parent "sup-1" :host host :interval-ms 5
                   :shout-fn (fn [agent event data]
                               (swap! shouts conj [agent event data])
                               (when (= :pane-idle event) (deliver heard-idle true))
                               true)})
    (let [idle? (deref heard-idle 5000 false)]
      (watch/job-ended! {:kind :compact :job-id "cc-1" :name "peer seven" :pane "%7" :status :done})
      (let [st (watch/status)]
        (watch/stop!)
        (is idle? "the idle shout arrived")
        (is (:running? st))
        (let [[agent event data] (first (filter #(= :pane-idle (second %)) @shouts))]
          (is (= "tmux:peer seven" agent))
          (is (= :pane-idle event))
          (is (= "sup-1" (:parent-id data)))
          (is (= "%7" (:tmux-pane data)))
          (is (string? (:message data))))
        (let [[_ _ job] (first (filter #(= :tmux-job (second %)) @shouts))]
          (is (= "done" (:tmux-job-status job)))
          (is (= "sup-1" (:parent-id job))))))
    (is (false? (:running? (watch/status))))))

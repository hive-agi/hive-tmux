(ns hive-tmux.claude.idle-capacity-test
  "S1 idle-capacity (card 20261122160543-4df00c21). Every port is a stub map
   (pulse, board counts, clock); no with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.idle-capacity :as ic]
            [hive-tmux.claude.sense :as sense]
            [hive-tmux.claude.watch :as watch]))

(def sid "72216f39-aaaa-bbbb-cccc-000000000001")

(defn- row [m]
  (merge {:pane "%3" :target "1:3.1" :name "coord" :session-id sid :pid 4242
          :cwd "/srv/hive" :ctx-tokens 120000 :busy? false :status :idle} m))

(def min-ms 60000)

(defn- ports
  "Stub ports: PULSE is the beat rows any coordinator id reads (or a fn of
   the ids), COUNTS the board counts. Records every pulse read in CALLS."
  [{:keys [pulse counts calls] :or {pulse [] counts {:todo 3 :doing 1}}}]
  {:pulse (fn [ids] (some-> calls (swap! conj ids)) (if (fn? pulse) (pulse ids) pulse))
   :open-cards (fn [_row] counts)})

(defn- run
  "Feed [[now rows] ...] through ic/step; the events as [now coordinator]."
  [p ticks & {:keys [opts] :or {opts {}}}]
  (loop [states {} [[now rows] & more] ticks out []]
    (if-not now
      out
      (let [{s :states evs :events} (ic/step p states rows now opts)]
        (recur s more (into out (map (fn [[k r]] [k now (get-in r [:idle-capacity :coordinator])]) evs)))))))

(deftest coordinator-ids-come-from-the-session-id
  (is (= ["coordinator:72216f39" "coordinator:4242"] (ic/coordinator-ids (row {}))))
  (is (= [] (ic/coordinator-ids (row {:session-id nil}))) "an unjoined pane is nobody's coordinator"))

(deftest fires-once-after-ten-idle-minutes-with-no-working-ling-and-open-cards
  (is (= [[:idle-capacity (* 11 min-ms) "coordinator:72216f39"]]
         (run (ports {}) [[0 [(row {})]] [(* 5 min-ms) [(row {})]] [(* 11 min-ms) [(row {})]]
                          [(* 20 min-ms) [(row {})]] [(* 40 min-ms) [(row {})]]]))
      "at most once per idle spell")
  (testing "the payload names coordinator, idle-ms, live-lings and open-cards"
    (let [{evs :events} (ic/step (ports {:pulse [{"agent" "ling-1" "state" "ask"}]})
                                 {sid {:idle-since 0}} [(row {})] (* 11 min-ms) {})
          [[k r]] evs
          d (ic/event-data r)]
      (is (= :idle-capacity k))
      (is (= {:tmux-coordinator "coordinator:72216f39" :tmux-idle-ms (* 11 min-ms)
              :tmux-live-lings 1 :tmux-open-cards 4 :tmux-open-todo 3 :tmux-open-doing 1} d))
      (is (re-find #"20261003154522-36473755" (ic/message r)))))
  (testing "a new working spell re-arms it"
    (is (= 2 (count (run (ports {}) [[0 [(row {})]] [(* 11 min-ms) [(row {})]]
                                     [(* 12 min-ms) [(row {:busy? true})]]
                                     [(* 13 min-ms) [(row {})]] [(* 24 min-ms) [(row {})]]]))))))

(deftest never-fires-on-a-human-draft-or-a-waiting-pane
  (is (empty? (run (ports {}) [[0 [(row {:input "half a thought"})]]
                               [(* 30 min-ms) [(row {:input "half a thought"})]]]))
      "text at the prompt: a human is typing")
  (is (empty? (run (ports {}) [[0 [(row {:status :waiting})]] [(* 30 min-ms) [(row {:status :waiting})]]]))
      "a :waiting session waits on a human's answer")
  (is (empty? (run (ports {}) [[0 [(row {:dialog {:kind :permission}})]]
                               [(* 30 min-ms) [(row {:dialog {:kind :permission}})]]])))
  (testing "a draft restarts the idle clock"
    (is (empty? (run (ports {}) [[0 [(row {})]] [(* 9 min-ms) [(row {:input "x"})]]
                                 [(* 15 min-ms) [(row {})]]])))))

(deftest a-working-ling-or-an-unreadable-pulse-holds-it
  (is (empty? (run (ports {:pulse [{"agent" "ling-1" "state" "working"}]})
                   [[0 [(row {})]] [(* 11 min-ms) [(row {})]]])))
  (is (empty? (run (ports {:pulse nil}) [[0 [(row {})]] [(* 11 min-ms) [(row {})]]]))
      "zero lings is not proven when the pulse cannot be read")
  (is (= 1 (count (run (ports {:pulse [{"agent" "tmux:peer" "state" "working"}]})
                       [[0 [(row {})]] [(* 11 min-ms) [(row {})]]])))
      "the pane watcher's own beats are panes, not lings"))

(deftest an-empty-or-unreadable-board-holds-it
  (is (empty? (run (ports {:counts {:todo 0 :doing 0}}) [[0 [(row {})]] [(* 11 min-ms) [(row {})]]])))
  (is (empty? (run (ports {:counts nil}) [[0 [(row {})]] [(* 11 min-ms) [(row {})]]]))))

(deftest the-pulse-is-read-per-coordinator-id-and-rechecked-at-most-each-minute
  (let [calls (atom [])
        p (ports {:calls calls :pulse (fn [ids] (if (some #{"coordinator:72216f39"} ids)
                                                 [{"agent" "ling-1" "state" "working"}] []))})]
    (is (empty? (run p [[0 [(row {})]] [(* 5 min-ms) [(row {})]] [(* 11 min-ms) [(row {})]]
                        [(+ (* 11 min-ms) 1000) [(row {})]] [(* 13 min-ms) [(row {})]]])))
    (is (= [["coordinator:72216f39" "coordinator:4242"] ["coordinator:72216f39" "coordinator:4242"]] @calls)
        "never before the threshold, then once per recheck window")))

(deftest coordinators-are-matched-by-session-id-not-pane-id
  (testing "a new session in a reused pane id starts a fresh clock"
    (is (empty? (run (ports {}) [[0 [(row {})]]
                                 [(* 11 min-ms) [(row {:session-id "0badf00d-new"})]]]))))
  (testing "the same session moving to another pane keeps its clock"
    (is (= 1 (count (run (ports {}) [[0 [(row {})]] [(* 11 min-ms) [(row {:pane "%9" :target "2:1.1"})]]])))))
  (testing "panes with no session, and Codex panes, are no coordinators"
    (is (empty? (run (ports {}) [[0 [(row {:session-id nil}) (row {:provider :codex})]]
                                 [(* 11 min-ms) [(row {:session-id nil}) (row {:provider :codex})]]])))))

(deftest the-sense-class-and-detail-fields-are-declared
  (is (= :sense/idle-capacity (sense/event->class :idle-capacity)))
  (is (= "idle_ms" (sense/detail-keys :tmux-idle-ms)))
  (is (= "open_cards" (sense/detail-keys :tmux-open-cards)))
  (is (= "live_lings" (sense/detail-keys :tmux-live-lings)))
  (is (= "coordinator" (sense/detail-keys :tmux-coordinator))))

(deftest the-boundary-ports-resolve-softly
  (let [resolve-fn {ic/pulse-sym (fn [{:keys [receptor]}]
                                   [{"agent" (str "ling-of-" (:parent receptor)) "state" "working"}])
                    ic/board-summary-sym (fn [pid] {:counts {:todo 2 :inprogress 1 :done 9} :pid pid})
                    ic/project-id-sym (fn [dir] (when (= "/srv/hive" dir) "hive"))}
        p (ic/default-ports resolve-fn)]
    (is (= [{"agent" "ling-of-coordinator:a" "state" "working"}] ((:pulse p) ["coordinator:a"])))
    (is (= {:todo 2 :doing 1} ((:open-cards p) (row {}))))
    (is (nil? ((:open-cards p) (row {:cwd "/elsewhere"}))) "no project, no board"))
  (let [p (ic/default-ports (constantly nil))]
    (is (nil? ((:pulse p) ["coordinator:a"])) "without sixth-sense the pulse is unreadable")
    (is (nil? ((:open-cards p) (row {}))))))

(deftest a-running-watcher-shouts-idle-capacity-to-its-parent
  (let [shouts (atom [])
        heard (promise)
        clock (atom 0)
        host {:tmux! (fn tmux!
                       ([argv] (tmux! argv nil))
                       ([argv _]
                        (case (first argv)
                          "list-panes" {:exit 0 :out "%3\t1\t3\t1\t9\tclaude\t/srv/hive\t✳ coord\n" :err ""}
                          "capture-pane" {:exit 0 :err ""
                                          :out "✻ Worked for 1s\n   120000 tokens\n──── coord ─\n❯\u00a0\n"})))
              :sessions (fn [] [{:sessionId sid :name "coord" :cwd "/srv/hive" :pid 4242 :status "idle"}])
              :transcript (fn [_ _] nil) :sleep! (fn [_]) :now-ms (fn [] 0)}]
    (watch/start! {:parent "coordinator:6fe2d4c2" :host host :interval-ms 5
                   :idle-ports (assoc (ports {}) :now-ms (fn [] (swap! clock + (* 6 min-ms))))
                   :shout-fn (fn [agent event data]
                               (swap! shouts conj [agent event data])
                               (when (= :idle-capacity event) (deliver heard true))
                               true)})
    (let [ok? (deref heard 5000 false)]
      (watch/stop!)
      (is ok? "the idle-capacity shout arrived")
      (let [[_ _ data] (first (filter #(= :idle-capacity (second %)) @shouts))]
        (is (= "coordinator:6fe2d4c2" (:parent-id data)))
        (is (= "coordinator:72216f39" (:tmux-coordinator data)))
        (is (= sid (:tmux-session-id data)))
        (is (= 4 (:tmux-open-cards data)))
        (is (= 0 (:tmux-live-lings data)))
        (is (< (* 10 min-ms) (:tmux-idle-ms data))))
      (is (= 1 (count (filter #(= :idle-capacity (second %)) @shouts)))))))

(ns assay-compact-interrupt
  "hive-assay experiment: does `tmux compact interrupt=auto` recycle a /goal
   pane whose one turn never ends, where the old job times out?

   Arms (factor :params/arm):
     :old-job               interrupt=false: the job as shipped before (wait for
                            a turn boundary, 30 min idle timeout)
     :interrupt-force-send  interrupt=auto, via force-send (/compact queued,
                            then C-x C-s)
     :interrupt-escape      interrupt=auto, via escape (Esc, then /compact)

   Unit: one scripted fake /goal pane (`hive-tmux.claude.goal-pane`) per
   seed. Its timeline, drawn from the seed (shared by all arms: paired), is a
   coordinator in one endless turn: 100 s `ss wait` calls, thinking gaps, a
   `sleep`, and with p=0.15 per step a `ship land` (a WRITE) of 1-10 min. With
   p=0.25 per pane a single turn boundary occurs somewhere in the first 3 h.

   Pass = compacted AND goal restored AND no write cut by a key.
   :outcome/latency-ms is the FAKE-clock time from job start to /compact sent
   (time to compact), or the job's end when it never compacted.

   Scenario :queued-human (`*scenario*`, pane %8, job cc-a49d0119): the
   operator queued 1-3 messages into the turn and the goal has only a
   goal_status line. Pass also needs every queued message delivered.

   Run (warm REPL with hive-assay on the classpath):
     (load-file \"dev/assay_compact_interrupt.clj\")
     (assay-compact-interrupt/run! {:replicates 20})
     (assay-compact-interrupt/run! {:replicates 20 :scenario :queued-human})"
  (:refer-clojure :exclude [run!])
  (:require [hive-assay.adapters.mock-transport :as mt]
            [hive-assay.domain :as d]
            [hive-assay.run :as run]
            [hive-assay.stats :as stats]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.goal-pane :as gp]))

(def poll-ms 5000)

(def ^:dynamic *write-p*
  "Share of timeline steps that are a write (`ship land`)."
  0.15)

(defn timeline
  "[[start-ms end-ms tool-or-kw] ...] for SEED over 4 h. tool: :ss-wait :sleep
   :ship-land nil (thinking) or :boundary (one rest between two turns)."
  [seed]
  (let [rnd (java.util.Random. (long seed))
        horizon (* 4 3600 1000)
        boundary-at (when (< (.nextDouble rnd) 0.25) (long (* (.nextDouble rnd) 3 3600 1000)))]
    (loop [t 0 acc [] b boundary-at]
      (if (>= t horizon)
        acc
        (if (and b (>= t b))
          (recur (+ t poll-ms) (conj acc [t (+ t poll-ms) :boundary]) nil)
          (let [x (.nextDouble rnd)
                w *write-p*
                [tool dur] (cond (< x w) [:ship-land (+ 60000 (.nextInt rnd 540000))]
                                 (< x (+ w 0.10)) [:sleep (+ 20000 (.nextInt rnd 80000))]
                                 (< x (+ w 0.30)) [nil (+ 5000 (.nextInt rnd 25000))]
                                 :else [:ss-wait 100000])]
            (recur (+ t dur) (conj acc [t (+ t dur) tool]) b)))))))

(defn- at [tl clock] (some (fn [[s e tool]] (when (and (<= s clock) (< clock e)) [tool])) tl))

(defn tick-fn
  "Drive the pane by TL until the job sends /compact; then leave it to the
   pane's own key reactions. Records :compact-at (fake clock)."
  [tl]
  (fn [s]
    (cond
      (some #(= "/compact" %) (:sent s)) (cond-> s (nil? (:compact-at s)) (assoc :compact-at (:clock s)))
      ;; An interrupted turn stays stopped (measured: recycle-goal-pane.sh).
      ;; A human message queued before the job does not stop the timeline.
      (or (:escaped s) (some #{"/compact"} (:queued s)) (pos? (or (:compacting s) 0))) s
      :else (let [[tool] (or (at tl (:clock s)) [:ss-wait])]
              (if (= :boundary tool)
                (assoc s :busy? false :tool nil)
                (assoc s :busy? true :tool tool))))))

(def arm-opts
  {:old-job {:interrupt false}
   :interrupt-force-send {:interrupt :auto :interrupt-via :force-send}
   :interrupt-escape {:interrupt :auto :interrupt-via :escape}})

(def ^:dynamic *scenario*
  ":plain (the pane as above) or :queued-human: before the job starts, the
   operator SUBMITTED 1-3 short messages into the working turn (pane %8,
   job cc-a49d0119), so they wait in Claude Code's queue and the composer
   reads back the first one; and the /goal was set without a /goal entry,
   so only a goal_status line names it. Pass then also needs every queued
   message delivered (:human-delivered), in order."
  :plain)

(defn- queued-for [seed]
  (let [rnd (java.util.Random. (+ 7919 (long seed)))]
    (vec (for [i (range (inc (.nextInt rnd 3)))] (str "ct " seed "-" i)))))

(defn trial
  "Run one compact job of ARM on the fake pane of SEED. A plain map of facts."
  [arm seed]
  (let [tl (timeline seed)
        queued? (= :queued-human *scenario*)
        queued (when queued? (queued-for seed))
        {:keys [state host]} (gp/goal-pane (cond-> {:tick (tick-fn tl)}
                                             queued? (assoc :queued queued :recall-queued? true
                                                            :transcript ["{\"type\":\"x\"}"
                                                                         (gp/goal-status-line gp/goal-text)])))
        job (compact/start! "%9" (merge {:host host :sync? true :poll-ms poll-ms}
                                        (get arm-opts arm)))
        s @state]
    {:status (:status job) :reason (:reason job)
     :compacted? (boolean (:compacted? job))
     :goal-restored? (boolean (and (:goal-active-after? job)
                                   (some #(= (str "/goal " gp/goal-text) %) (:sent s))))
     :human-delivered? (or (not queued?) (= queued (:human-delivered s)))
     :write-cut (or (:write-cut s) 0)
     :interrupted-via (:interrupted-via job)
     :time-to-compact-ms (or (:compact-at s) (:clock s))
     :boundary? (boolean (some #(= :boundary (nth % 2)) tl))}))

(defn outcome [{:keys [compacted? goal-restored? human-delivered? write-cut time-to-compact-ms]}]
  (d/run-outcome {:outcome/status :completed
                  :outcome/turns 1
                  :outcome/events (cond-> #{}
                                    compacted? (conj :compacted)
                                    goal-restored? (conj :goal-restored)
                                    human-delivered? (conj :human-delivered)
                                    (zero? write-cut) (conj :no-write-cut))
                  :outcome/cost-usd 0.0
                  :outcome/latency-ms time-to-compact-ms}))

(def base
  (d/specimen {:specimen/statement "recycle a /goal pane whose one turn never ends"
               :specimen/model "fake-goal-pane"
               :specimen/tools #{}
               :specimen/params {:arm :old-job}
               :specimen/oracle (d/oracle {:oracle/kind :signal-coverage
                                           :expected #{:compacted :goal-restored :human-delivered :no-write-cut}})}))

(defn experiment [replicates]
  (d/experiment {:experiment/id "tmux-compact-interrupt"
                 :experiment/hypothesis "interrupt=auto recycles a never-resting /goal pane that the old job cannot, without cutting a write"
                 :experiment/base base
                 :experiment/factors {:params/arm (vec (keys arm-opts))}
                 :experiment/design :full-factorial
                 :experiment/replicates replicates}))

(defn- pctl [xs p] (when (seq xs) (nth (vec (sort xs)) (min (dec (count xs)) (long (* p (count xs)))))))

(defn run!
  "Run the experiment; answer {:experiment Result :table per-arm summary}."
  [{:keys [replicates scenario] :or {replicates 20 scenario :plain}}]
  (let [facts (atom [])
        transport (mt/mock-transport
                   (fn [sp {:keys [seed]}]
                     (let [arm (get-in sp [:specimen/params :arm])
                           f (binding [*scenario* scenario] (trial arm seed))]
                       (swap! facts conj (assoc f :arm arm :seed seed))
                       (outcome f))))
        res (run/run-experiment! transport (experiment replicates) {:concurrency 1})
        trials (get-in res [:ok :experiment/trials])
        by-arm (group-by #(get-in % [:trial/env-actual :params/arm]) trials)
        fby (group-by :arm @facts)]
    {:experiment res
     :table (into (sorted-map)
                  (for [[arm ts] by-arm
                        :let [n (count ts)
                              k (count (filter #(get-in % [:trial/verdict :verdict/pass?]) ts))
                              fs (get fby arm)
                              ok-times (map :time-to-compact-ms (filter :compacted? fs))]]
                    [arm {:n n :pass k :rate (double (/ k n))
                          :wilson-95 (stats/wilson-ci k n)
                          :write-cuts (reduce + (map :write-cut fs))
                          :reasons (frequencies (map (juxt :status :reason) fs))
                          :ttc-median-s (some-> (pctl ok-times 0.5) (quot 1000))
                          :ttc-p90-s (some-> (pctl ok-times 0.9) (quot 1000))
                          :ttc-max-s (some-> (seq ok-times) (->> (apply max)) (quot 1000))}]))}))

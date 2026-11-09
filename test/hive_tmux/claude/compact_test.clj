(ns hive-tmux.claude.compact-test
  "The compaction sequence against a scripted host: a fake pane whose screen
   reacts to what is typed into it. No tmux, no ~/.claude, no real clock."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.panes :as panes]
            [hive-tmux.claude.registry :as registry]
            [hive-tmux.claude.host :as host]))

(def goal-text "Work in a loop on funeraria: take card 0812fc24, then 2a255f97.")

(defn- goal-line [args]
  (json/write-str {:type "user"
                   :message {:role "user"
                             :content (str "<command-name>/goal</command-name>\n"
                                           "<command-message>goal</command-message>\n"
                                           "<command-args>" args "</command-args>")}}))

(defn- screen [{:keys [tokens busy-polls goal? human]}]
  [(if (pos? busy-polls) "✶ Compacting… (12s · ↓ 1k tokens)" "✻ Worked for 2s")
   (str "   " tokens " tokens" (when goal? " · ◎ /goal active (3m)"))
   "──────────── peer one ─"
   (str "❯ " human)
   "  -- INSERT -- ⏵⏵ auto mode on"])

(defn scripted-host
  "A host whose single pane %9 runs \"peer one\". Each submitted line makes the
   pane busy for two polls; /compact drops the tokens and clears the goal, and
   /goal sets it again. :sent records every submitted line in order."
  [{:keys [tokens goal? transcript after-submit] :or {after-submit (fn [s _line] s)}}]
  (let [st (atom {:tokens tokens :busy-polls 0 :goal? goal? :clock 0
                  :typed "" :sent [] :argv []})
        submit! (fn [line]
                  (swap! st (fn [s]
                              (-> (cond-> (-> s (update :sent conj line) (assoc :busy-polls 2 :typed ""))
                                    (str/starts-with? line "/compact") (assoc :tokens 30000 :goal? false)
                                    (str/starts-with? line "/goal ") (assoc :goal? true)
                                    (= line "hive workflow catchup /caveman") (update :tokens + 20000))
                                  (after-submit line)))))]
    {:state st
     :host
     {:tmux! (fn tmux!
               ([argv] (tmux! argv nil))
               ([argv _stdin]
                (swap! st update :argv conj argv)
                (case (first argv)
                  "list-panes" {:exit 0 :out "%9\t1\t3\t1\t100\tclaude\t/srv/peer\t✳ peer one\n" :err ""}
                  "capture-pane" (let [s @st]
                                   (when (pos? (:busy-polls s)) (swap! st update :busy-polls dec))
                                   {:exit 0 :out (str/join "\n" (screen s)) :err ""})
                  "send-keys" (let [[_ _ _ & ks] argv]
                                (cond (= ["Enter"] ks) (submit! (:typed @st))
                                      (= "-l" (first ks)) (swap! st assoc :typed (second ks)))
                                {:exit 0 :out "" :err ""})
                  {:exit 0 :out "" :err ""})))
      :sessions (fn [] [{:pid 200 :sessionId "sid-1" :name "peer one" :cwd "/srv/peer"
                         :status (if (pos? (:busy-polls @st)) "busy" "idle") :updatedAt 1}])
      :transcript (fn [cwd sid] (when (= ["/srv/peer" "sid-1"] [cwd sid]) transcript))
      :sleep! (fn [ms] (swap! st update :clock + ms))
      :now-ms (fn [] (:clock @st))}}))

(def fast {:poll-ms 1000 :settle-ms 2000 :sync? true})

(deftest compacts-reseeds-and-restores-the-goal
  (let [{:keys [state host]} (scripted-host {:tokens 450000 :goal? true
                                             :transcript ["{\"type\":\"x\"}" (goal-line goal-text)]})
        job (compact/start! "peer one" (assoc fast :host host))]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (= ["/compact" "hive workflow catchup /caveman" (str "/goal " goal-text)]
           (:sent @state))
        "the goal comes back AFTER the re-seed, because /compact cleared it")
    (is (= 450000 (:tokens-before job)))
    (is (= 50000 (:tokens-after job)))
    (is (true? (:goal-active-after? job)))
    (is (= goal-text (:goal job)))))

(defn- busy-forever-after-goal
  "The peer starts working under a new /goal at once and does not stop."
  [s line]
  (cond-> s (str/starts-with? line "/goal ") (assoc :busy-polls 1000000)))

(deftest the-last-send-ends-the-job-even-when-the-pane-keeps-working
  (testing "compact: the restored goal sets the peer to work, the job is still :done"
    (let [{:keys [state host]} (scripted-host {:tokens 450000 :goal? true
                                               :transcript [(goal-line goal-text)]
                                               :after-submit busy-forever-after-goal})
          ended (atom nil)
          job (compact/start! "%9" (assoc fast :host host :on-end #(reset! ended %)))]
      (is (= :done (:status job)) (pr-str (dissoc job :steps)))
      (is (true? (:started? job)))
      (is (true? (:goal-active-after? job)))
      (is (= (str "/goal " goal-text) (last (:sent @state))))
      (is (= :done (:status @ended)) "on-end fired, so the tmux-job sense can fire")))
  (testing "goal: the same for a goal set without compacting"
    (let [{:keys [host]} (scripted-host {:tokens 100000 :goal? false
                                         :after-submit busy-forever-after-goal})
          job (compact/start! "%9" (assoc fast :host host :kind :goal :goal "Take the next card."))]
      (is (= [:done true] [(:status job) (:started? job)]))))
  (testing "compact with no goal to restore: the reseed is the last send"
    (let [{:keys [state host]} (scripted-host {:tokens 450000 :goal? false
                                               :after-submit (fn [s line]
                                                               (cond-> s (= line "hive workflow catchup /caveman")
                                                                 (assoc :busy-polls 1000000)))})
          job (compact/start! "%9" (assoc fast :host host))]
      (is (= [:done true] [(:status job) (:started? job)]))
      (is (= ["/compact" "hive workflow catchup /caveman"] (:sent @state))))))

(deftest an-unacknowledged-last-send-is-done-not-started
  (let [{:keys [host]} (scripted-host {:tokens 100000 :goal? false
                                       :after-submit (fn [s line]
                                                       (cond-> s (str/starts-with? line "/goal ")
                                                         (assoc :busy-polls 0 :goal? false)))})
        job (compact/start! "%9" (assoc fast :host host :kind :goal :goal "Take the next card."))]
    (is (= :done (:status job)))
    (is (false? (:started? job)) "the pane never showed it working nor the goal active")))

(deftest under-threshold-is-skipped-unless-forced
  (let [{:keys [state host]} (scripted-host {:tokens 100000 :goal? false})]
    (is (= :skipped (:status (compact/start! "%9" (assoc fast :host host)))))
    (is (empty? (:sent @state)))
    (is (= :done (:status (compact/start! "%9" (assoc fast :host host :force? true)))))
    (is (= ["/compact" "hive workflow catchup /caveman"] (:sent @state))
        "no goal was active, so none is sent")))

(deftest a-pane-that-never-settles-fails-instead-of-hanging
  (let [{:keys [state host]} (scripted-host {:tokens 450000 :goal? false})]
    (swap! state assoc :busy-polls 1000000)
    (let [job (compact/start! "%9" (assoc fast :host host :idle-timeout-ms 20000))]
      (is (= :failed (:status job)))
      (is (= {:during :wait-idle} (:error job)))
      (is (empty? (:sent @state)) "nothing is typed into a working pane"))))

(deftest a-cleared-goal-is-not-restored
  (is (nil? (panes/goal-text [(goal-line goal-text) (goal-line "clear")])))
  (is (= goal-text (panes/goal-text [(goal-line "old") (goal-line goal-text)]))))

(deftest sweep-dry-run-names-only-panes-over-threshold
  (let [{:keys [state host]} (scripted-host {:tokens 450000 :goal? false})
        h-params {:command "compact" :dry_run true}]
    (binding [host/*host* host] (let [r (registry/dispatch h-params)]
        (is (= ["%9"] (mapv :pane (get-in r [:ok :would-compact]))))
        (is (empty? (get-in (registry/dispatch (assoc h-params :exclude "peer one")) [:ok :would-compact])))
        (is (empty? (get-in (registry/dispatch (assoc h-params :threshold 500000)) [:ok :would-compact])))))
    (is (empty? (:sent @state)) "a dry run types nothing")))

(deftest text-a-human-left-at-the-prompt-is-never-typed-over
  (let [{:keys [state host]} (scripted-host {:tokens 450000 :goal? true
                                             :transcript [(goal-line goal-text)]})]
    (swap! state assoc :human "hive workflow catchup /caveman")
    (let [job (compact/start! "%9" (assoc fast :host host))]
      (is (= :skipped (:status job)))
      (is (= :pending-human-input (:reason job)))
      (is (= "hive workflow catchup /caveman" (get-in job [:error :input])))
      (is (empty? (:sent @state)) "not one key was sent")
      (is (not-any? #(= "send-keys" (first %)) (:argv @state))))))

(deftest a-supervisor-can-replace-or-extend-the-goal
  (testing "goal replaces the captured one"
    (let [{:keys [state host]} (scripted-host {:tokens 450000 :goal? true
                                               :transcript [(goal-line goal-text)]})]
      (compact/start! "%9" (assoc fast :host host :goal "Take card 7b90138c."))
      (is (= "/goal Take card 7b90138c." (last (:sent @state))))))
  (testing "goal_append extends the captured one"
    (let [{:keys [state host]} (scripted-host {:tokens 450000 :goal? true
                                               :transcript [(goal-line goal-text)]})]
      (compact/start! "%9" (assoc fast :host host :goal-append "Then keep pulling cards."))
      (is (= (str "/goal " goal-text " Then keep pulling cards.") (last (:sent @state))))))
  (testing "a goal can be set on a pane that had none"
    (let [{:keys [state host]} (scripted-host {:tokens 450000 :goal? false})]
      (compact/start! "%9" (assoc fast :host host :goal "Start the loop."))
      (is (= "/goal Start the loop." (last (:sent @state)))))))

(deftest say-submits-one-prompt-and-ends-when-the-pane-starts-on-it
  (let [{:keys [state host]} (scripted-host {:tokens 100000 :goal? true})
        ended (atom nil)
        job (compact/start! "%9" (assoc fast :host host :kind :say :text "close the loop: run wrap"
                                        :on-end #(reset! ended %)))]
    (is (= :done (:status job)))
    (is (= :say (:kind job)))
    (is (true? (:delivered job)))
    (is (= ["close the loop: run wrap"] (:sent @state)) "no /compact, no re-seed, only the prompt")
    (is (= (:job-id job) (:job-id @ended)) "on-end saw the finished job")))

(deftest say-never-types-over-pending-input
  (let [{:keys [state host]} (scripted-host {:tokens 100000 :goal? false})]
    (swap! state assoc :human "half a sentence")
    (let [job (compact/start! "%9" (assoc fast :host host :kind :say :text "hello"))]
      (is (= [:skipped :pending-human-input] [(:status job) (:reason job)]))
      (is (empty? (:sent @state))))))

(deftest goal-arms-a-goal-without-compacting
  (let [{:keys [state host]} (scripted-host {:tokens 100000 :goal? false})
        job (compact/start! "%9" (assoc fast :host host :kind :goal :goal "Take the next card."))]
    (is (= :done (:status job)))
    (is (= ["/goal Take the next card."] (:sent @state)))
    (is (true? (:goal-active-after? job))))
  (testing "goal_append needs an active goal or an explicit one"
    (let [{:keys [host]} (scripted-host {:tokens 100000 :goal? false})
          job (compact/start! "%9" (assoc fast :host host :kind :goal :goal-append "more"))]
      (is (= "more" (:goal job)) "append alone becomes the goal when none is active"))))

(deftest an-unknown-kind-is-refused
  (let [{:keys [host]} (scripted-host {:tokens 1 :goal? false})]
    (is (= :unknown-kind (:error (compact/start! "%9" (assoc fast :host host :kind :dance)))))))

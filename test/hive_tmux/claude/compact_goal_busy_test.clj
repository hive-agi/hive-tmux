(ns hive-tmux.claude.compact-goal-busy-test
  "Card 20261119233707-70541f83: goal and compact jobs on busy /goal panes,
   and a `say` that reports a real dialog instead of waiting on it.

   recorded-busy-goal and recorded-idle are live 2.1.328 panes (read only,
   scrubbed of private data); the dialog fixtures and
   recorded-needs-input-esc-text were recorded from a scratch Claude Code
   2.1.329 on a private tmux server (-L). The hosts are stubs: no tmux, no
   ~/.claude, no real clock. Nothing is sent to a real pane."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.screen :as screen]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def goal "Work in a loop: take the next card, then the one after it.")

(def reseed compact/default-reseed)

(defn- goal-line [args]
  (json/write-str {:type "user"
                   :message {:role "user"
                             :content (str "<command-name>/goal</command-name>\n"
                                           "<command-message>goal</command-message>\n"
                                           "<command-args>" args "</command-args>")}}))

(defn fixture [name]
  (str/split-lines (slurp (io/resource (str "hive_tmux/claude/fixtures/" name ".ansi")))))

(defn- host-from-fixture
  "A host whose screen is a recorded capture. Typing is recorded under :typed
   and :sent, never sent to a tmux server. ON-SUBMIT (state line -> state)
   applies a submitted line the way Claude Code would."
  ([lines] (host-from-fixture lines {}))
  ([lines {:keys [on-submit status transcript] :or {on-submit (fn [s _] s) status "idle"}}]
   (let [st (atom {:lines (vec lines) :typed "" :sent [] :clock 0 :transcript (vec transcript)})
         prompt-row (some (fn [i] (when (str/starts-with? (str/triml (screen/strip-ansi (nth lines i))) "\u276f") i))
                          (range (dec (count lines)) -1 -1))
         type! #(swap! st update :typed str %)]
     {:state st
      :host {:tmux! (fn tmux!
                      ([argv] (tmux! argv nil))
                      ([argv stdin]
                       (case (first argv)
                         "list-panes" {:exit 0 :err ""
                                       :out "%7\t1\t1\t1\t4242\tclaude\t/srv/peer\t\u2733 peer one\n"}
                         "capture-pane" (let [s @st
                                              ls (cond-> (:lines s)
                                                   (and prompt-row (seq (:typed s)))
                                                   (assoc prompt-row (str "\u276f " (:typed s))))]
                                          {:exit 0 :err "" :out (str/join "\n" ls)})
                         "load-buffer" (do (swap! st assoc :buffer stdin) {:exit 0 :out "" :err ""})
                         "paste-buffer" (do (type! (:buffer @st)) {:exit 0 :out "" :err ""})
                         "send-keys" (let [[_ _ _ & ks] argv]
                                       (cond (= ["Enter"] ks)
                                             (swap! st (fn [s]
                                                         (let [line (:typed s)]
                                                           (on-submit (-> s (update :sent conj line) (assoc :typed ""))
                                                                      line))))
                                             (= "-l" (first ks)) (type! (second ks)))
                                       {:exit 0 :out "" :err ""})
                         {:exit 0 :out "" :err ""})))
             :sessions (fn [] [{:pid 4242 :sessionId "sid-1" :name "peer one"
                                :cwd "/srv/peer" :status status :updatedAt 1}])
             :transcript (fn [_ _] (:transcript @st))
             :sleep! (fn [ms] (swap! st update :clock + ms))
             :now-ms (fn [] (:clock @st))}})))

(defn- draw-goal
  "ON-SUBMIT: `/goal <text>` resets the footer's goal age to 0s and writes
   the command entry to the transcript, as Claude Code does."
  [s line]
  (if (and (str/starts-with? line "/goal ") (not= line "/goal clear"))
    (-> s
        (update :lines (fn [ls] (mapv #(str/replace % #"/goal active \([^)]*\)" "/goal active (0s)") ls)))
        (update :transcript conj (goal-line (subs line 6))))
    s))

(defn- without-goal-footer [lines]
  (mapv #(str/replace % #" · \S*◎ /goal active \([^)]*\)" "") lines))

(def ^:private fast {:poll-ms 1000 :settle-ms 2000 :sync? true})

;;; goal: no idle wait, confirmed on its own footer

(deftest goal-on-a-busy-recorded-pane-never-waits-for-idle
  (let [{:keys [state host]} (host-from-fixture (fixture "recorded-busy-goal-2.1.328") {:on-submit draw-goal})
        job (compact/start! "%7" (assoc fast :host host :kind :goal :goal "take the next card"))]
    (is (:busy? (screen/read-screen (fixture "recorded-busy-goal-2.1.328"))) "the pane is working")
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (true? (:started? job)))
    (is (true? (:goal-active-after? job)))
    (is (= ["/goal take the next card"] (:sent @state)))
    (is (not-any? #(= :wait-idle (:step %)) (:steps job)))
    (is (< (:clock @state) 10000) "set at once, not after a wait")))

(deftest a-goal-is-done-only-on-its-own-footer
  (testing "footer never drawn"
    (let [{:keys [state host]} (host-from-fixture (without-goal-footer (fixture "recorded-busy-goal-2.1.328")))
          job (compact/start! "%7" (assoc fast :host host :kind :goal :goal "take card 2" :start-timeout-ms 3000))]
      (is (= [:failed :goal-unconfirmed] [(:status job) (:reason job)]) (pr-str (dissoc job :steps)))
      (is (= ["/goal take card 2"] (:sent @state)))))
  (testing "an old goal's footer, not redrawn, nothing in the transcript"
    (let [{:keys [host]} (host-from-fixture (fixture "recorded-busy-goal-2.1.328"))
          job (compact/start! "%7" (assoc fast :host host :kind :goal :goal "take card 2" :start-timeout-ms 3000))]
      (is (= [:failed :goal-unconfirmed] [(:status job) (:reason job)]))))
  (testing "the goal's entry lands in the transcript"
    (let [{:keys [host]} (host-from-fixture (fixture "recorded-busy-goal-2.1.328")
                                            {:on-submit (fn [s line] (update s :transcript conj (goal-line (subs line 6))))})
          job (compact/start! "%7" (assoc fast :host host :kind :goal :goal "take card 2" :start-timeout-ms 3000))]
      (is (= :done (:status job)) (pr-str (dissoc job :steps))))))

(deftest goal-refuses-a-screen-it-cannot-read
  (testing "no prompt row (mid-redraw)"
    (let [lines (vec (remove #(str/starts-with? (str/triml (screen/strip-ansi %)) "❯")
                             (fixture "recorded-idle-2.1.328")))
          {:keys [state host]} (host-from-fixture lines)
          job (compact/start! "%7" (assoc fast :host host :kind :goal :goal "take card 2"))]
      (is (= [:skipped :unreadable-screen] [(:status job) (:reason job)]) (pr-str job))
      (is (empty? (:sent @state)))))
  (testing "a recorded AskUserQuestion menu"
    (let [{:keys [state host]} (host-from-fixture (fixture "recorded-ask-user-question-2.1.329"))
          job (compact/start! "%7" (assoc fast :host host :kind :goal :goal "take card 2"))]
      (is (= [:skipped :open-dialog] [(:status job) (:reason job)]))
      (is (= "" (:typed @state))))))

;;; say: a dialog is reported, reply text is not a dialog

(deftest recorded-dialogs-are-reported-not-waited-out
  (doseq [name ["recorded-permission-dialog-2.1.329" "recorded-ask-user-question-2.1.329"
                "recorded-trust-dialog-2.1.329"]]
    (let [lines (fixture name)
          {:keys [state host]} (host-from-fixture lines)
          job (compact/start! "%7" (assoc fast :host host :kind :say :text "use staging"))]
      (is (some? (:dialog (screen/read-screen lines))) name)
      (is (= [:skipped :open-dialog] [(:status job) (:reason job)]) name)
      (is (= "esc to cancel" (get-in job [:error :dialog :evidence])) name)
      (is (< (:clock @state) 1000) (str name ": reported at once"))
      (is (empty? (:sent @state)) name))))

(deftest reply-text-is-not-a-dialog
  ;; Friction 20261119234010-02a476db: the reply ends in `Needs input:` and
  ;; `Press Esc to cancel`, the composer below it is empty: say is ready.
  (let [lines (fixture "recorded-needs-input-esc-text-2.1.329")
        {:keys [state host]} (host-from-fixture lines)
        job (compact/start! "%7" (assoc fast :host host :kind :say :text "use staging"))]
    (is (nil? (:dialog (screen/read-screen lines))))
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (= ["use staging"] (:sent @state)))))

(deftest a-mark-in-scrollback-above-an-open-composer-is-not-a-dialog
  (let [lines (conj (vec (butlast (fixture "recorded-idle-2.1.328")))
                    "  -- INSERT -- opening… · esc to cancel")]
    (is (nil? (screen/dialog-of lines)) "the composer is open, so this is no dialog")))

;;; compact clear_goal: clear while busy, compact, reseed acknowledged, goal back

(defn- pane-screen [{:keys [tokens goal? goal-age rest? typed]}]
  [(if rest? "✻ Worked for 3m 12s" "✶ Percolating… (26m 48s · ↓ 29.8k tokens)")
   (str "  " tokens " tokens" (when goal? (str " · ◎ /goal active (" (or goal-age "26m") ")")))
   "──────────── peer one ─"
   (str "\u001b[39m❯ " typed)
   "───────────"
   "  -- INSERT -- ⏵⏵ auto mode on"])

(defn- submit [s line]
  (let [s (-> s (update :sent conj line) (assoc :typed ""))]
    (cond
      (= line "/goal clear") (cond-> s (:clears? s) (assoc :goal? false :busy-turn? false))
      (str/starts-with? line "/goal ") (-> s (assoc :goal? true :goal-age "0s")
                                           (update :transcript conj (goal-line (subs line 6))))
      (str/starts-with? line "/compact") (assoc s :tokens 21000 :goal? false)
      (= line reseed) (-> s (update :tokens + 20000) (assoc :busy-turn? true))
      :else s)))

(defn- stub
  "Pane %7 under a /goal whose Stop hook keeps it working: it never rests
   while the goal is active. CLEARS? false: `/goal clear` is never acted on."
  [{:keys [clears?] :or {clears? true}}]
  (let [st (atom {:tokens 469000 :goal? true :typed "" :sent [] :clock 0 :clears? clears?
                  :transcript [(goal-line goal)] :busy-turn? true})
        read! #(swap! st (fn [s] (assoc s :rest? (and (not (:goal? s)) (not (:busy-turn? s))))))]
    {:state st
     :host
     {:tmux! (fn tmux!
               ([argv] (tmux! argv nil))
               ([argv stdin]
                (case (first argv)
                  "list-panes" {:exit 0 :err "" :out "%7\t1\t1\t1\t4242\tclaude\t/srv/peer\t✳ peer one\n"}
                  "capture-pane" {:exit 0 :err "" :out (str/join "\n" (pane-screen (read!)))}
                  "load-buffer" (do (swap! st assoc :buffer stdin) {:exit 0 :out "" :err ""})
                  "paste-buffer" (do (swap! st update :typed str (:buffer @st)) {:exit 0 :out "" :err ""})
                  "send-keys" (let [[_ _ _ & ks] argv]
                                (cond (= ["Enter"] ks) (swap! st submit (:typed @st))
                                      (= "-l" (first ks)) (swap! st update :typed str (second ks)))
                                {:exit 0 :out "" :err ""})
                  {:exit 0 :out "" :err ""})))
      :sessions (fn [] [{:pid 4242 :sessionId "sid-1" :name "peer one" :cwd "/srv/peer"
                         :status "idle" :updatedAt 1}])
      :transcript (fn [cwd sid] (when (= ["/srv/peer" "sid-1"] [cwd sid]) (:transcript @st)))
      :sleep! (fn [ms] (swap! st update :clock + ms))
      :now-ms (fn [] (:clock @st))}}))

(deftest clear-goal-compacts-a-busy-goal-pane-and-ends-on-the-acknowledged-reseed
  (let [{:keys [state host]} (stub {})
        job (compact/start! "%7" (assoc fast :host host :clear-goal? true :interrupt false
                                        :idle-timeout-ms 60000))]
    (is (= :done (:status job)) (pr-str (dissoc job :steps)))
    (is (= ["/goal clear" "/compact" reseed (str "/goal " goal)] (:sent @state)))
    (is (true? (:compacted? job)))
    (is (true? (:reseed-acknowledged? job)) "the reseed was seen working; no settled idle after it")
    (is (true? (:started? job)) "the restored goal showed on its own footer")
    (is (true? (:goal-active-after? job)))
    (is (nil? (:restore job)) "nothing had to be sent back on a stop")))

(deftest a-clear-that-is-never-acted-on-sends-the-goal-back
  (let [{:keys [state host]} (stub {:clears? false})
        job (compact/start! "%7" (assoc fast :host host :clear-goal? true :interrupt false
                                        :idle-timeout-ms 20000 :start-timeout-ms 3000))]
    (is (= [:failed :timeout] [(:status job) (:reason job)]) (pr-str (dissoc job :steps)))
    (is (= :goal-clear (get-in job [:error :during])))
    (is (= ["/goal clear" (str "/goal " goal)] (:sent @state)) "no /compact without a proven clear")
    (is (= goal (get-in job [:restore :goal])))))

(deftest without-clear-goal-the-job-is-unchanged
  (let [{:keys [state host]} (stub {})
        job (compact/start! "%7" (assoc fast :host host :interrupt false :idle-timeout-ms 20000))]
    (is (= [:failed :timeout] [(:status job) (:reason job)]))
    (is (empty? (:sent @state)) "nothing typed into the working pane")))

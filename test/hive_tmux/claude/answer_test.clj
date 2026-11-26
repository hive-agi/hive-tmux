(ns hive-tmux.claude.answer-test
  "Screen-driven menu answers against recorded captures and a scripted host.
   No tmux server, Claude state directory or wall clock is reached."
  (:require [clojure.data.json :as json]
            [clojure.edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-dsl.result :as r]
            [hive-tmux.claude.answer :as answer]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.registry :as registry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- fixture
  "Fixture NAME's lines. `answer-real-*` are real Claude Code 2.1.x screens:
   .txt ones were recorded from live panes through `swarm tmux read` (escapes
   already stripped), the .ansi one with `capture-pane -p -e`. The others are
   hand-made in the same layout."
  [name]
  (let [r (or (io/resource (str "hive_tmux/claude/fixtures/" name ".ansi"))
              (io/resource (str "hive_tmux/claude/fixtures/" name ".txt")))]
    (str/split-lines (slurp r))))

(def ^:private audit-log (atom []))

(defn- audit! [record] (swap! audit-log conj record))

(defn- with-audit [f]
  (reset! audit-log [])
  (binding [answer/*audit!* audit!] (f)))

(use-fixtures :each with-audit)

(def ^:private real-ask (fixture "answer-real-ask-user-question"))
(def ^:private real-bash (fixture "answer-real-bash-guard-dialog"))
(def ^:private real-grant (fixture "answer-real-dont-ask-again-dialog"))
(def ^:private real-scrollback (fixture "answer-real-dialog-words-in-scrollback"))

(def ^:private ask (fixture "answer-ask-user-question"))
(def ^:private permission (fixture "answer-permission-dialog"))
(def ^:private scrolled (fixture "answer-scrolled-menu"))
(def ^:private no-menu (fixture "answer-no-menu"))
(def ^:private answered (fixture "answer-answered"))

(defn- menu [screen] (:menu (answer/parse-menu screen)))

(defn- refusal [screen] (:error (answer/parse-menu screen)))

(deftest real-claude-screens-parse-as-menus
  (testing "a recorded AskUserQuestion: the free-text rows after a rule stay in the block"
    (let [m (menu real-ask)]
      (is (= :ask-user-question (:kind m)))
      (is (= 1 (:highlighted m)))
      (is (= ["Compact now" "Keep going" "Type something." "Chat about this"] (mapv :label (:options m))))
      (is (str/includes? (:question m) "pause for compaction?"))))
  (testing "a recorded hive-guard Bash dialog is a permission dialog whose body holds the command"
    (let [m (menu real-bash)]
      (is (= :permission-dialog (:kind m)))
      (is (= ["Yes" "No"] (mapv :label (:options m))))
      (is (str/includes? (:question m) "Do you want to proceed?"))
      (is (str/includes? (:question m) "Locate built-in selection in autonomous"))))
  (testing "a recorded dialog offering a standing grant"
    (let [m (menu real-grant)]
      (is (= :permission-dialog (:kind m)))
      (is (= [1 2 3] (mapv :number (:options m))))))
  (testing "hand-made screens in the same layout"
    (is (= :ask-user-question (:kind (menu ask))))
    (is (= 2 (:highlighted (menu ask))))
    (is (= :permission-dialog (:kind (menu permission))))
    (is (= [3 4 5 6 7] (mapv :number (:options (menu scrolled)))))
    (is (= 5 (:highlighted (menu scrolled))))))

(deftest a-real-capture-with-escapes-and-dialog-words-in-scrollback-is-no-menu
  (is (some #(str/includes? % "\u001b[") real-scrollback) "the fixture keeps its escapes")
  (is (nil? (menu real-scrollback))))

(def ^:private probes
  "The Fable review's probe screens (c01-c15, E1), as refusals or kinds."
  {:c01 "● Here are the steps:\n  1. Install the thing\n> 2. Configure it\n  3. Run it\n"
   :c08 "● Plan:\n  1. read file\n  2. edit file\n\n  Do you want to proceed?\n❯ 1. Yes\n  2. No\n"
   :c10 "  Which features?\n❯ 1. [ ] Alpha\n  2. [✔] Beta\n  3. [ ] Gamma\n  4. Submit\n\nEnter to select · ↑/↓ to navigate · Esc to cancel"
   :c11 "$ cat notes\n> 1. buy milk\n2. buy eggs\n"
   :c13 "  Edit file src/foo.clj\n  Do you want to make this edit to foo.clj?\n❯ 1. Yes\n  2. Yes, allow all edits during this session (shift+tab)\n  3. No\n\n Esc to cancel"
   :c14 "  Should I ask for permission first?\n❯ 1. Yes\n  2. No\n\nEnter to select · ↑/↓ to navigate · Esc to cancel"
   :c15 "  Bash command\n    rm -rf build\n  This command requires approval\n❯ 1. Yes\n  2. Yes, and don't ask again for: rm:*\n  3. No\n\n Esc to cancel"
   :e1 (str "● Steps I will take:\n  1. Foo\n  2. Bar\n\n────────────────\n❯ 3. also do baz\n"
            "────────────────\n  ? for shortcuts\n")
   :e1-footer (str "  Pick one\n❯ 1. A\n  2. B\n\nEnter to select · ↑/↓ to navigate · Esc to cancel\n"
                   "────────────────\n❯ draft\n────────────────\n  -- INSERT --\n")
   :digits "  Pick\n❯ 12345678901234567890. huge\n  2. B\n\n Esc to cancel"
   :gap "  Pick\n❯ 1. A\n  3. C\n\n Esc to cancel"
   :lonely "  Pick\n❯ 1. A\n\n Esc to cancel"
   :two-highlights "  Pick\n❯ 1. A\n❯ 2. B\n\n Esc to cancel"})

(deftest screens-that-are-not-one-live-menu-are-refused-with-a-reason
  (doseq [[probe reason] {:c01 :no-menu :c08 :not-a-menu :c11 :no-menu :e1 :not-a-menu
                          :c10 :multi-select :e1-footer :composer-visible
                          :digits :no-highlight :gap :not-a-menu :lonely :not-a-menu
                          :two-highlights :no-highlight}]
    (is (= reason (refusal (probes probe))) (name probe)))
  (doseq [screen [no-menu nil [] "$ ls\n1. a file\n2. another file"]]
    (is (= :no-menu (refusal screen)) (pr-str screen))))

(deftest kind-fails-closed-to-permission-dialog
  (is (= :permission-dialog (:kind (menu (probes :c13)))) "an edit dialog")
  (is (= :permission-dialog (:kind (menu (probes :c15)))) "approval wording")
  (is (= :ask-user-question (:kind (menu (probes :c14))))
      "a question that says 'permission' but carries the AskUserQuestion footer")
  (testing "c08 with a footer: the reply list above is not part of the dialog"
    (let [m (menu (str (probes :c08) "\n Esc to cancel"))]
      (is (= ["Yes" "No"] (mapv :label (:options m))))
      (is (= :no-such-option (:error (answer/select-option m "read file")))))))

(deftest option-selection-is-exact-and-never-guesses
  (let [menu (menu ask)]
    (is (= :no-such-option (:error (answer/select-option menu "12345678901234567890"))))
    (is (= 3 (get-in (answer/select-option menu 3) [:option :number])))
    (is (= 2 (get-in (answer/select-option menu "  continue WITH the   current task ")
                     [:option :number])))
    (is (= :no-such-option (:error (answer/select-option menu 9))))
    (is (= :no-such-option (:error (answer/select-option menu "continue"))))
    (is (= :missing-option (:error (answer/select-option menu " "))))
    (is (= :ambiguous-option
           (:error (answer/select-option (assoc menu :options
                                                [{:number 1 :label "Yes"}
                                                 {:number 2 :label "yes"}]) "Yes"))))))

(defn- render
  "MENU lines with the `❯` moved to the option row numbered HIGHLIGHT, the way
   Claude Code redraws a menu after an arrow key."
  [menu highlight]
  (mapv (fn [line]
          (if-let [[_ ind mark n rest] (re-matches #"^( *)(❯ +)?([0-9]{1,2})(\. .*)$" line)]
            (let [width (+ (count ind) (count mark))
                  pad (apply str (repeat (max 0 (- width 2)) \space))]
              (if (= (str highlight) n)
                (str pad "❯ " n rest)
                (str (apply str (repeat width \space)) n rest)))
            line))
        menu))

(defn- scripted-host
  "One fake Claude pane. MENU is shown, its highlight following the arrows,
   until Enter; then NEXT (default ANSWERED) is shown. AFTER-ARROWS, when
   given, replaces the menu as soon as an arrow key arrives (a menu that
   changed under the caller). The composer handles tmux -l text separately
   from named keys, including read-back and /compact. Compaction adds a
   transcript boundary, the real job's completion proof."
  [{:keys [menu highlight answered next after-arrows gate fail-key]}]
  (let [state (atom {:keys [] :clock 0 :highlight highlight :menu menu})
        tmux! (fn [argv]
                (case (first argv)
                  "capture-pane"
                  (let [{:keys [typed menu highlight]} @state]
                    {:exit 0 :err ""
                     :out (cond typed (str "────────\n❯ " typed "\n────────\n  -- INSERT --\n")
                                menu (str/join "\n" (render menu highlight))
                                :else (str/join "\n" (or next answered)))})
                  "list-panes" {:exit 0 :out "%9\t1\t3\t1\t100\tclaude\t/srv/peer\t✳ peer one\n" :err ""}
                  "send-keys"
                  (let [key (last argv)]
                    (cond
                      (= fail-key key) {:exit 1 :out "" :err "no server"}
                      (= "-l" (nth argv 3 nil)) (do (swap! state assoc :typed key) {:exit 0 :out "" :err ""})
                      :else
                      (do (swap! state update :keys conj key)
                          (cond
                            (contains? #{"Up" "Down"} key)
                            (swap! state #(cond-> (update % :highlight + (if (= "Up" key) -1 1))
                                            after-arrows (assoc :menu after-arrows)))
                            (= "Enter" key)
                            (if (:typed @state)
                              (swap! state assoc :typed nil :tokens 30000
                                     :transcript [{:type "system" :subtype "compact_boundary" :post-tokens 30000}])
                              (swap! state assoc :menu nil)))
                          {:exit 0 :out "" :err ""})))
                  "load-buffer" {:exit 0 :out "" :err ""}
                  "paste-buffer" {:exit 0 :out "" :err ""}))]
    {:state state
     :host {:tmux! tmux!
            :sessions (fn [] [{:pid 200 :sessionId "sid-1" :name "peer one"
                               :cwd "/srv/peer" :status "idle" :updatedAt 1}])
            :transcript (fn [_ _]
                          (mapv #(json/write-str %) (:transcript @state)))
            :sleep! (fn [ms]
                      (when (and gate (= 1 ms))
                        (deliver (:entered gate) true)
                        @(:released gate))
                      (swap! state update :clock + ms))
            :now-ms (fn [] (:clock @state))}}))

(defn- run-answer [screen start option opts & {:as host-opts}]
  (let [{:keys [state host]} (scripted-host (merge {:menu screen :highlight start :answered answered} host-opts))]
    {:result (answer/answer! host {:pane "%9"} option (merge {:actor "coordinator:test"} opts))
     :keys (:keys @state)}))

(deftest question-menus-are-answered-with-arrows-a-reread-and-enter
  (doseq [[screen start option expected-keys number label]
          [[ask 2 1 ["Up" "Enter"] 1 "Pause for compaction"]
           [real-ask 1 "Keep going" ["Down" "Enter"] 2 "Keep going"]
           [scrolled 5 7 ["Down" "Down" "Enter"] 7 "Stop and report"]
           [scrolled 5 "Repair the parser" ["Up" "Enter"] 4 "Repair the parser"]]]
    (let [{:keys [result keys]} (run-answer screen start option {})]
      (is (= {:delivered? true :confirmed? true :option number :label label}
             (select-keys result [:delivered? :confirmed? :option :label])) (pr-str result))
      (is (= expected-keys keys))
      (is (= expected-keys (:keys (peek @audit-log))) "audited without any job on the pane")
      (is (= "coordinator:test" (:actor (peek @audit-log)))))))

(deftest permission-dialogs-need-permission-and-expect
  (testing "refused by default, with the question and options to relay"
    (let [{:keys [result keys]} (run-answer real-bash 1 1 {})]
      (is (= :permission-dialog (:error result)))
      (is (= ["Yes" "No"] (mapv :label (get-in result [:menu :options]))))
      (is (str/includes? (get-in result [:menu :question]) "Do you want to proceed?"))
      (is (= [] keys))
      (is (= :permission-dialog (:outcome (peek @audit-log))) "a refusal is audited too")))
  (testing "permission=true without expect is refused"
    (is (= :expect-required (:error (:result (run-answer real-bash 1 1 {:permission? true}))))))
  (testing "expect must occur in the dialog body"
    (let [{:keys [result keys]} (run-answer real-bash 1 1 {:permission? true :expect "rm -rf"})]
      (is (= :expect-mismatch (:error result)))
      (is (= [] keys))))
  (testing "with both, it is answered"
    (let [{:keys [result keys]} (run-answer real-bash 1 1 {:permission? true :expect "Locate built-in selection"})]
      (is (true? (:delivered? result)) (pr-str result))
      (is (= ["Enter"] keys))))
  (testing "c13 and c15 are permission dialogs too"
    (doseq [p [:c13 :c15]]
      (is (= :permission-dialog (:error (:result (run-answer (str/split-lines (probes p)) 1 1 {})))) (name p)))))

(defn- grant-dialog
  "A permission dialog for `rm -rf build` whose option 2 reads LABEL."
  [label]
  (str/split-lines (str "  Bash command\n    rm -rf build\n  Do you want to proceed?\n❯ 1. Yes\n  2. "
                        label "\n  3. No, and tell Claude what to do differently\n\n Esc to cancel")))

(deftest standing-grants-are-never-chosen
  (testing "the recorded and hand-made grant rows"
    (doseq [[screen option expect] [[real-grant 2 "hive — Code commands"]
                                    [permission "Yes, and don't ask again for this command" "run the test suite"]
                                    [(str/split-lines (probes :c13)) 2 "src/foo.clj"]
                                    [(str/split-lines (probes :c15)) 2 "rm -rf build"]]]
      (let [{:keys [result keys]} (run-answer screen 1 option {:permission? true :expect expect})]
        (is (= :persistent-grant (:error result)) (pr-str result))
        (is (= [] keys)))))
  (testing "an allow-list: any permission label but yes, no or no, ... is refused"
    (doseq [label ["Yes, and auto-accept edits" "Yes, and bypass permissions" "Yes, I trust this folder"
                   "Yes, allow reading from etc/ from this project" "Yes, and add /etc as a working directory"
                   "Yes, and don’t ask again for rm:*" "Yes, sure"]]
      (let [{:keys [result keys]} (run-answer (grant-dialog label) 1 2 {:permission? true :expect "rm -rf build"})]
        (is (= :persistent-grant (:error result)) label)
        (is (= [] keys) label))))
  (testing "one-shot answers stay allowed"
    (doseq [option [1 3]]
      (let [{:keys [result]} (run-answer (grant-dialog "Yes, and bypass permissions") 1 option
                                         {:permission? true :expect "rm -rf build"})]
        (is (true? (:delivered? result)) (pr-str result))))))

(deftest expect-must-quote-the-dialog-not-its-boilerplate
  (let [rm (str/split-lines "  Bash command\n    rm -rf ~/PP\n  Do you want to proceed?\n❯ 1. Yes\n  2. No\n\n Esc to cancel")]
    (doseq [generic ["Do you want to proceed?" "proceed" "do you want to"]]
      (let [{:keys [result keys]} (run-answer rm 1 1 {:permission? true :expect generic})]
        (is (= :expect-too-generic (:error result)) generic)
        (is (= [] keys) generic)))
    (is (true? (:delivered? (:result (run-answer rm 1 1 {:permission? true :expect "rm -rf ~/PP"})))))))

(deftest multi-select-menus-send-nothing
  (let [{:keys [result keys]} (run-answer (str/split-lines (probes :c10)) 1 4 {})]
    (is (= :multi-select (:error result)))
    (is (= [] keys))))

(deftest composer-drafts-and-reply-lists-send-nothing
  (doseq [p [:e1 :c08 :c01]]
    (let [{:keys [result keys]} (run-answer (str/split-lines (probes p)) 1 1 {})]
      (is (false? (:delivered? result)) (name p))
      (is (= [] keys) (name p)))))

(def ^:private dialog-a
  (str/split-lines "  Bash command\n    git status\n  Do you want to proceed?\n❯ 1. Yes\n  2. No\n\n Esc to cancel"))

(def ^:private dialog-b
  (str/split-lines "  Bash command\n    rm -rf ~/PP\n  Do you want to proceed?\n❯ 1. Yes\n  2. No\n\n Esc to cancel"))

(deftest a-menu-that-changes-after-the-arrows-gets-no-enter
  (testing "E3/E4: a different menu, or none, after the arrows"
    (doseq [replacement [dialog-a answered]]
      (let [{:keys [result keys]} (run-answer ask 2 1 {} :after-arrows replacement)]
        (is (= :menu-changed (:error result)) (pr-str result))
        (is (= ["Up"] (:keys-sent result)))
        (is (= ["Up"] keys) "Enter never went out")
        (is (false? (:delivered? result))))))
  (testing "a failed send names the keys already delivered"
    (let [{:keys [result keys]} (run-answer ask 2 3 {} :fail-key "Enter")]
      (is (= :key-delivery-failed (:error result)))
      (is (= ["Down"] (:keys-sent result)))
      (is (= ["Down"] keys)))))

(deftest after-enter-the-result-never-claims-nothing-happened
  (testing "E2: the next dialog shows as next-menu, delivered? stays true"
    (let [{:keys [result keys]} (run-answer dialog-a 1 1 {:permission? true :expect "git status"} :next dialog-b)]
      (is (= {:delivered? true :confirmed? false} (select-keys result [:delivered? :confirmed?])) (pr-str result))
      (is (str/includes? (get-in result [:next-menu :question]) "rm -rf ~/PP"))
      (is (= ["Enter"] keys))))
  (testing "the same menu still showing: delivered but unconfirmed"
    (let [{:keys [result]} (run-answer ask 2 2 {:opts {:confirm-tries 0}} :next ask)]
      (is (= {:delivered? true :confirmed? false} (select-keys result [:delivered? :confirmed?]))))))

(deftest answer-composes-with-a-real-queued-compaction-job
  ;; A compact job refuses to START on a pane showing a menu (`open-dialog?`
  ;; stops it :skipped :open-dialog, the S3 human-draft guard). The
  ;; composition pinned here is the one that guard leaves: the job is already
  ;; waiting when a menu opens. So the pane starts idle with no menu, the gate
  ;; holds the job inside its first wait, and only then does the menu open
  ;; and get answered.
  (let [entered (promise)
        released (promise)
        gate {:entered entered :released released}
        {:keys [state host]} (scripted-host {:menu nil :highlight 2 :answered answered :gate gate})
        queued (compact/start! "%9" {:host host :force? true :reseed ""
                                     :poll-ms 1 :settle-ms 1})
        job-id (:job-id queued)
        before (compact/jobs)
        entered? (deref entered 10000 false)
        _ (is (true? entered?) "the real job is waiting, not merely scheduled")
        _ (swap! state assoc :menu ask :highlight 2)
        answer-result (answer/answer! host {:pane "%9"} 1 {:actor "coordinator:400"})
        during (compact/job job-id)]
    (is (true? (:delivered? answer-result)) (pr-str answer-result))
    (is (= job-id (:job-id answer-result)))
    (is (not= :already-running (:error answer-result)))
    (is (= {:step :answer :actor "coordinator:400" :pane "%9" :outcome :confirmed
            :kind :ask-user-question :option 1 :label "Pause for compaction"}
           (dissoc (last (:steps during)) :at :keys :question)))
    (is (= ["Up" "Enter"] (:keys (last (:steps during)))))
    (is (contains? #{:queued :running} (:status during)))
    (deliver released true)
    (let [finished (loop [n 10000]
                     (let [job (compact/job job-id)]
                       (cond (= :done (:status job)) job
                             (zero? n) job
                             :else (do (Thread/sleep 1) (recur (dec n))))))]
      (is (= :done (:status finished)) (pr-str finished @state))
      (is (= job-id (:job-id finished)))
      (is (= 1 (count (filter #(= :answer (:step %)) (:steps finished)))))
      (is (= (count before) (count (compact/jobs))) "no extra answer job took a registry slot")
      (is (= ["Up" "Enter"] (take 2 (:keys @state)))))))

(defn- dispatch-on [screen params]
  (let [{:keys [state host]} (scripted-host {:menu screen :highlight 2 :answered answered})]
    (binding [host/*host* host]
      {:result (registry/dispatch (merge {:command "answer" :pane "%9" :option "1"} params))
       :keys (:keys @state)})))

(deftest the-swarm-verb-checks-who-calls-and-which-pane
  (testing "the caller's own pane (pid 200 joins pane %9)"
    (let [{:keys [result keys]} (dispatch-on ask {:_caller_id "coordinator:200"})]
      (is (= :tmux/own-pane (:error result)))
      (is (= [] keys))))
  (testing "no caller id"
    (let [{:keys [result keys]} (dispatch-on ask {})]
      (is (= :tmux/no-caller (:error result)))
      (is (= [] keys))))
  (testing "a permission dialog is refused through the verb, with what to relay"
    (let [{:keys [result keys]} (dispatch-on real-bash {:_caller_id "coordinator:400"})]
      (is (= :tmux/permission-dialog (:error result)))
      (is (= ["Yes" "No"] (mapv :label (get-in result [:menu :options]))))
      (is (= [] keys))))
  (testing "permission and expect pass through as params"
    (let [{:keys [result keys]} (dispatch-on real-bash {:_caller_id "coordinator:400" :permission "true"
                                                        :expect "Locate built-in selection"})]
      (is (r/ok? result) (pr-str result))
      (is (= ["Up" "Enter"] keys) "the scripted host started the highlight on row 2")))
  (testing "an AskUserQuestion menu is answered for a peer"
    (let [{:keys [result keys]} (dispatch-on ask {:_caller_id "coordinator:400"})]
      (is (true? (get-in result [:ok :delivered?])))
      (is (= ["Up" "Enter"] keys))))
  (testing "no menu: refused, no job started"
    (let [before (count (compact/jobs))
          {:keys [result]} (dispatch-on no-menu {:_caller_id "coordinator:400"})]
      (is (= :tmux/no-menu (:error result)))
      (is (= before (count (compact/jobs))))))
  (is (= [:pane :option :permission :expect :socket_name]
         (get-in (registry/dispatch {:command "help"}) [:ok :commands "answer" :params]))))

(deftest a-pane-ref-matching-two-rows-is-refused
  (let [{:keys [state host]} (scripted-host {:menu ask :highlight 2 :answered answered})
        two (assoc host :tmux! (fn [argv]
                                 (if (= "list-panes" (first argv))
                                   {:exit 0 :err ""
                                    :out (str "%9\t1\t3\t1\t100\tclaude\t/srv/peer\t✳ peer one\n"
                                              "%10\t1\t4\t1\t101\tclaude\t/srv/peer\t✳ peer one\n")}
                                   ((:tmux! host) argv))))]
    (binding [host/*host* two]
      (let [result (registry/dispatch {:command "answer" :pane "peer one" :option "1"
                                       :_caller_id "coordinator:400"})]
        (is (= :tmux/ambiguous-pane (:error result)))
        (is (= ["%10" "%9"] (sort (map :pane (:matches result)))))
        (is (= [] (:keys @state)))))))

(deftest only-an-explicit-true-enables-permission
  (doseq [v ["off" "" "yes" "1" "on" false nil]]
    (let [{:keys [result keys]} (dispatch-on real-bash {:_caller_id "coordinator:400" :permission v
                                                        :expect "Locate built-in selection"})]
      (is (= :tmux/permission-dialog (:error result)) (pr-str v))
      (is (= [] keys) (pr-str v))))
  (doseq [v [true "true" " TRUE "]]
    (is (r/ok? (:result (dispatch-on real-bash {:_caller_id "coordinator:400" :permission v
                                                :expect "Locate built-in selection"})))
        (pr-str v))))

(deftest no-key-goes-out-unless-the-intent-is-audited
  (testing "the sink refuses before the first key: :audit-unavailable, nothing sent"
    (let [{:keys [result keys]} (run-answer ask 2 1 {:audit! (fn [_] (throw (ex-info "disk full" {})))})]
      (is (= :audit-unavailable (:error result)))
      (is (= "disk full" (:audit-error result)))
      (is (= [] keys))))
  (testing "the outcome write fails after Enter: delivered? stays true, audit-failed is said"
    (let [n (atom 0)
          sink (fn [r] (when (= 2 (swap! n inc)) (throw (ex-info "disk full" {}))) (audit! r))
          {:keys [result keys]} (run-answer ask 2 1 {:audit! sink})]
      (is (= {:delivered? true :audit-failed true} (select-keys result [:delivered? :audit-failed])))
      (is (= ["Up" "Enter"] keys))
      (is (= [:answer-intent] (mapv :step @audit-log)) "the intent was on record before the keys"))))

(deftest append-audit-writes-one-edn-line-per-record
  (let [dir (java.nio.file.Files/createTempDirectory "answer-audit" (make-array java.nio.file.attribute.FileAttribute 0))
        f (io/file (str dir) "nested" "answers.edn")]
    (binding [answer/*audit-file* f
              answer/*audit!* answer/append-audit!]
      (run-answer ask 2 1 {:audit! nil}))
    (let [records (mapv clojure.edn/read-string (str/split-lines (slurp f)))]
      (is (= [:answer-intent :answer] (mapv :step records)))
      (is (= ["Up" "Enter"] (:keys (peek records))))
      (is (= "coordinator:test" (:actor (peek records)))))))
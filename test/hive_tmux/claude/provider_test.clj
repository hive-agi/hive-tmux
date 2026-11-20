(ns hive-tmux.claude.provider-test
  "Mixed assistant terminals must not acquire another provider's control state."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-dsl.result :as r]
            [hive-addon.protocol :as addon]
            [hive-tmux.init :as init]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.panes :as panes]
            [hive-tmux.claude.registry :as registry]))

(defn- stub-host [commands]
  (let [calls (atom [])
        terminal ["\u001b[31mVisible Codex output\u001b[0m"
                  "   450000 tokens · \u25ce /goal active (3m)"
                  "──────────── peer one ─"
                  "\u276f unfinished human text"]
        rows (map-indexed
              (fn [index command]
                (str "%" (+ 9 index) "\t1\t" (+ 3 index) "\t1\t"
                     (+ 100 index) "\t" command "\t/srv/peer\tpeer one"))
              commands)]
    {:calls calls
     :host {:tmux! (fn [argv]
                     (swap! calls conj argv)
                     (case (first argv)
                       "list-panes" {:exit 0 :out (str/join "\n" rows)}
                       "capture-pane" {:exit 0 :out (str/join "\n" terminal)}
                       (throw (ex-info "Unexpected terminal mutation" {:argv argv}))))
            :sessions (fn []
                        (swap! calls conj [:sessions])
                        [{:pid 200 :sessionId "claude-only-session"
                          :name "peer one" :cwd "/srv/peer" :status "idle"
                          :updatedAt 1}])
            :transcript (fn [& args]
                          (swap! calls conj (into [:transcript] args))
                          ["{\"type\":\"assistant\",\"message\":{\"content\":\"CLAUDE-ONLY-TRANSCRIPT\"}}"])
            :sleep! (fn [_] (throw (ex-info "Unexpected control wait" {})))
            :now-ms (constantly 0)}}))

(deftest foreground-command-determines-provider
  (doseq [command [nil "" "bash" "node" "my-codex" "codex-helper" "claude"]]
    (is (false? (panes/codex-pane? {:command command :title "codex"}))))
  (is (true? (panes/codex-pane? {:command "codex" :title "plain title"}))))

(deftest public-inventory-includes-codex-and-filters-by-provider
  (let [{:keys [host]} (stub-host ["claude" "codex" "bash"])]
    (binding [host/*host* host]
      (is (= [[:claude "%9"] [:codex "%10"]]
             (mapv (juxt :provider :pane) (get-in (registry/dispatch {:command "panes"}) [:ok :panes]))))
      (doseq [[provider expected] [["codex" ["%10"]] [:claude ["%9"]] ["all" ["%9" "%10"]]]]
        (is (= expected (mapv :pane (get-in (registry/dispatch {:command "panes" :pane_provider provider})
                                           [:ok :panes])))))
      (is (r/err? (registry/dispatch {:command "panes" :pane_provider "unknown"}))))))

(deftest codex-inventory-does-not-read-claude-state-or-capture-screen
  (let [{:keys [host calls]} (stub-host ["codex"])
        row (first (panes/panes host {:provider :codex :include-goal? true}))]
    (is (= :codex (:provider row)))
    (is (= :unknown (:status row)))
    (is (= #{:read-screen} (:capabilities row)))
    (is (= "100" (:pane-pid row)))
    (is (= "peer one" (:title row)))
    (doseq [key [:name :session-id :job-id :pid :busy? :ctx-tokens :goal-active? :goal :input :suggestion]]
      (is (not (contains? row key)) (name key)))
    (is (= ["list-panes"] (mapv first @calls)))))

(deftest matching-title-and-cwd-do-not-join-codex-to-claude-transcript
  (let [{:keys [host calls]} (stub-host ["claude" "codex"])]
    (binding [host/*host* host]
      (let [result (registry/dispatch {:command "read" :pane "%10" :lines 4 :last_assistant true})
            observed (:ok result)]
        (is (= :codex (:provider observed)))
        (is (= :unknown (:status observed)))
        (is (= :terminal (:screen-format observed)))
        (is (= ["Visible Codex output" "   450000 tokens · \u25ce /goal active (3m)"
                "──────────── peer one ─" "\u276f unfinished human text"]
               (:screen observed)))
        (is (not (contains? observed :last-assistant)))
        (is (not (contains? observed :busy?)))
        (is (not (contains? observed :goal-active?)))
        (is (not-any? #(= :transcript (first %)) @calls))))))

(deftest codex-control-refusals-start-no-job-even-with-force
  (let [{:keys [host calls]} (stub-host ["claude" "codex"])
        started (atom [])]
    (binding [host/*host* host]
      (with-redefs [compact/start! (fn [& args] (swap! started conj args) {:status :queued})]
        (doseq [parameters [{:command "say" :pane "%10" :text "hello" :wait_idle false}
                            {:command "goal" :pane "%10" :goal "work" :wait_idle false}
                            {:command "compact" :pane "%10" :force true}
                            {:command "compact" :pane "%9,%10" :force true}
                            {:command "compact" :pane "%10" :dry_run true}]]
          (is (r/err? (registry/dispatch parameters)) (pr-str parameters)))
        (is (empty? @started))))
    (is (not-any? #(= "send-keys" (first %)) @calls))))

(deftest automation-default-and-sweep-remain-claude-only
  (let [{:keys [host]} (stub-host ["claude" "codex"])]
    (is (= ["%9"] (mapv :pane (panes/panes host))))
    (binding [host/*host* host]
      (let [result (registry/dispatch {:command "compact" :dry_run true :threshold 1})]
        (is (not (r/err? result)))
        (is (= ["%9"] (mapv :pane (get-in result [:ok :would-compact]))))))))

(deftest direct-automation-cannot-observe-codex-as-claude
  (let [{:keys [host calls]} (stub-host ["codex"])
        result (compact/start! "%9" {:host host :sync? true :force? true})]
    (is (= :failed (:status result)))
    (is (not-any? #(= "send-keys" (first %)) @calls))))

(deftest invalid-provider-and-missing-pane-errors-are-actionable
  (let [{:keys [host]} (stub-host ["claude" "codex"])]
    (binding [host/*host* host]
      (doseq [given ["" "ALL" false 42 [] :unknown]]
        (is (= :tmux/unsupported-provider
               (:error (registry/dispatch {:command "panes" :pane_provider given})))
            (pr-str given)))
      (let [result (registry/dispatch {:command "compact" :pane "%404" :dry_run true})]
        (is (= :tmux/no-such-pane (:error result)))
        (is (some #{"%10"} (:known result)))
        (is (every? string? (:known result)))))))

(deftest claude-say-and-goal-still-reach-the-job-boundary
  (let [{:keys [host]} (stub-host ["claude" "codex"])
        started (atom [])]
    (binding [host/*host* host]
      (with-redefs [compact/start! (fn [ref opts]
                                    (swap! started conj [ref (select-keys opts [:kind :text :goal])])
                                    {:status :queued})]
        (doseq [parameters [{:command "say" :pane "%9" :text "hello"}
                            {:command "goal" :pane "%9" :goal "work"}]]
          (is (not (r/err? (registry/dispatch parameters)))))
        (is (= [["%9" {:kind :say :text "hello"}]
                ["%9" {:kind :goal :goal "work"}]]
               @started))))))

(deftest addon-port-exports-pane-filter-without-a-model-provider-extension
  (let [a (init/addon-ctor {})
        extension (get (addon/schema-extensions a) "swarm")
        provider-enum (get-in extension ["pane_provider" :enum])]
    (doseq [provider ["all" "claude" "codex"]]
      (is (some #{provider} provider-enum)))
    (is (not (contains? extension "provider")))
    (is (= {} (addon/hooks a)))))

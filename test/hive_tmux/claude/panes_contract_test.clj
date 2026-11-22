(ns hive-tmux.claude.panes-contract-test
  "Contracts the 2026-11-20 mutation pilot showed untested in
   hive-tmux.claude.panes (kill-rate 0.34): the tmux argv and format, the row
   shapes, provider filtering, pane lookup, the session join, and the
   transcript readers."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.panes :as panes]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private format-fields
  ["#{pane_id}" "#{session_name}" "#{window_index}" "#{pane_index}" "#{pane_pid}"
   "#{pane_current_command}" "#{pane_current_path}" "#{pane_title}" "#{pane_tty}"])

(defn- tsv [& cols] (str/join "\t" cols))

(def ^:private claude-line (tsv "%1" "s" "0" "1" "100" "claude" "/w" "" "/dev/pts/1"))
(def ^:private codex-line  (tsv "%2" "s" "0" "2" "200" "codex" "/c" "t" "/dev/pts/2"))
(def ^:private shell-line  (tsv "%3" "s" "0" "3" "300" "zsh" "/z" "" "/dev/pts/3"))

(defn- host
  "A fake host over LINES of list-panes output, recording every tmux argv."
  [calls lines & {:keys [sessions transcript] :or {sessions [] transcript (constantly [])}}]
  {:tmux!      (fn [argv]
                 (swap! calls conj argv)
                 (if (= "list-panes" (first argv))
                   {:exit 0 :out (str/join "\n" lines)}
                   {:exit 0 :out ""}))
   :sessions   (fn [] sessions)
   :transcript transcript})

;; ---------------------------------------------------------------- tmux argv

(deftest list-raw-asks-tmux-for-every-field
  (let [calls (atom [])]
    (panes/list-raw (host calls [claude-line]))
    (is (= [["list-panes" "-a" "-F" (str/join "\t" format-fields)]] @calls))))

(deftest list-raw-failure-says-why
  (let [e (try (panes/list-raw {:tmux! (fn [_] {:exit 1 :err "no server\n"})})
               (catch clojure.lang.ExceptionInfo e e))]
    (is (= {:type :tmux/list-failed :exit 1} (ex-data e)))
    (is (= "tmux list-panes failed: no server" (ex-message e)))))

(deftest capture-argv-and-failure
  (let [calls (atom [])
        h     {:tmux! (fn [argv] (swap! calls conj argv) {:exit 0 :out "a  \nb\n"})}]
    (is (= ["a" "b" ""] (panes/capture h "%3" 40)))
    (is (= [["capture-pane" "-p" "-e" "-t" "%3" "-S" "-40"]] @calls)))
  (is (nil? (panes/capture {:tmux! (fn [_] {:exit 1 :out "x"})} "%3" 40))))

;; ---------------------------------------------------------------- pane kinds

(deftest which-panes-are-claude-and-codex
  (is (true? (panes/claude-pane? {:command "claude"})))
  (is (true? (panes/claude-pane? {:command "2.1.319"})) "the Claude Code version as process title")
  (is (not (panes/claude-pane? {:command ""})))
  (is (not (panes/claude-pane? {:command "codex"})))
  (is (true? (panes/codex-pane? {:command "codex"})))
  (is (false? (panes/codex-pane? {:command "claude"}))))

(deftest a-codex-row-is-read-only-identity
  (is (= {:pane "%2" :target "s:0.2" :cwd "/c" :title "t" :command "codex" :pane-pid "200"
          :provider :codex :status :unknown :capabilities #{:read-screen} :unjoined? true}
         (first (panes/panes (host (atom []) [codex-line]) {:provider :codex})))))

;; ---------------------------------------------------------------- providers

(deftest providers-filter-the-panes
  (let [h (host (atom []) [claude-line codex-line shell-line])]
    (is (= ["%1"] (mapv :pane (panes/panes h))) "default: Claude only")
    (is (= ["%1"] (mapv :pane (panes/panes h {:provider :claude}))))
    (is (= ["%2"] (mapv :pane (panes/panes h {:provider :codex}))))
    (is (= ["%1" "%2"] (mapv :pane (panes/panes h {:provider :all}))))
    (is (= {:provider :x}
           (ex-data (try (panes/panes h {:provider :x}) (catch clojure.lang.ExceptionInfo e e)))))))

(deftest a-claude-row-carries-its-capabilities
  (let [row (first (panes/panes (host (atom []) [claude-line])))]
    (is (= :claude (:provider row)))
    (is (= #{:read-screen :say :compact :goal :answer} (:capabilities row)))))

(deftest panes-captures-the-requested-lines
  (let [calls (atom [])]
    (panes/panes (host calls [claude-line]) {:lines 7})
    (is (= ["capture-pane" "-p" "-e" "-t" "%1" "-S" "-7"] (second @calls)))))

;; ---------------------------------------------------------------- lookup

(deftest pane-finds-by-id-target-session-id-and-name
  (let [sessions [{:name "alpha" :cwd "/w" :sessionId "sid-1" :status "idle" :updatedAt 1}]
        h        (host (atom []) [(tsv "%1" "s" "0" "1" "100" "claude" "/w" "alpha" "/dev/pts/1")]
                       :sessions sessions)]
    (is (= "%1" (:pane (panes/pane h "%1"))))
    (is (= "%1" (:pane (panes/pane h "s:0.1"))))
    (is (= "%1" (:pane (panes/pane h "sid-1"))))
    (is (= "%1" (:pane (panes/pane h "alpha"))))
    (is (nil? (panes/pane h "nope")))
    (is (= "%1" (:pane (panes/pane h "%1" {}))))))

;; ---------------------------------------------------------------- join and rows

(deftest join-session-prefers-the-cwd-and-flags-ambiguity
  (let [a {:name "n" :cwd "/a" :updatedAt 1}
        b {:name "n" :cwd "/b" :updatedAt 2}
        c {:name "n" :cwd "/b" :updatedAt 3}]
    (is (= {:session a :ambiguous? false} (panes/join-session [a b] "n" "/a")))
    (is (= {:session b :ambiguous? true} (panes/join-session [a b] "n" "/z"))
        "no cwd match: the newest of the named ones, ambiguous")
    (is (= {:session c :ambiguous? true} (panes/join-session [a b c] "n" "/b")))
    (is (= {:session a :ambiguous? false} (panes/join-session [a] "n" "/z")))
    (is (nil? (panes/join-session [a] "m" "/a")))))

(deftest pane-row-is-exact
  (let [pane    {:pane "%1" :target "s:0.1" :cwd "/w" :title "" :tty "/dev/pts/1"}
        facts   {:footer-name "alpha" :ctx-tokens 1200 :busy? false :goal-active? true
                 :goal-age "2m" :activity "Thinking" :vim-mode "INSERT" :input "hi" :suggestion "s"}
        session {:name "alpha" :cwd "/w2" :sessionId "sid" :jobId "job" :pid 42 :kind "bg"
                 :status "busy" :updatedAt 1}]
    (is (= {:pane "%1" :target "s:0.1" :name "alpha" :cwd "/w2" :session-id "sid" :job-id "job"
            :pid 42 :kind "bg" :status :busy :ctx-tokens 1200 :busy? true :goal-active? true
            :goal-ended? false :goal-age "2m" :activity "Thinking" :vim-mode "INSERT" :input "hi"
            :suggestion "s" :tty "/dev/pts/1"}
           (panes/pane-row pane facts [session])))
    (testing "unjoined: the first name stays, the pane's cwd is used, an empty tty is nil"
      (let [row (panes/pane-row (assoc pane :tty "") facts [])]
        (is (= "alpha" (:name row)))
        (is (= "/w" (:cwd row)))
        (is (true? (:unjoined? row)))
        (is (nil? (:tty row)))
        (is (false? (:busy? row)))))
    (testing "ambiguous joins are flagged"
      (is (true? (:ambiguous? (panes/pane-row pane facts [session (assoc session :cwd "/w3")])))))))

;; ---------------------------------------------------------------- transcripts

(defn- goal-line [args]
  (json/write-str {:type "user"
                   :message {:content (str "<command-name>/goal</command-name><command-args>" args
                                           "</command-args>")}}))

(deftest goal-text-reads-the-latest-goal
  (is (= "ship it" (panes/goal-text [(goal-line "old") (goal-line " ship it ")])))
  (doseq [off ["clear" "off" "stop" "STOP"]]
    (is (nil? (panes/goal-text [(goal-line "x") (goal-line off)])) off))
  (is (nil? (panes/goal-text [(goal-line "  ")])))
  (is (nil? (panes/goal-text [])))
  (testing "content given as parts"
    (is (= "parts" (panes/goal-text
                    [(json/write-str {:message {:content [{:type "text"
                                                           :text "<command-name>/goal</command-name><command-args>parts</command-args>"}]}})]))))
  (testing "a candidate line whose decoded content lacks the marker is skipped"
    (is (= "real" (panes/goal-text [(goal-line "real")
                                    (json/write-str {:message {:content "no marker"}
                                                     :raw "<command-name>/goal</command-name>"})])))))

(defn- assistant [content] (json/write-str {:type "assistant" :message {:content content}}))

(deftest last-assistant-text-skips-what-says-nothing
  (is (= "hello" (panes/last-assistant-text [(assistant "hello")])))
  (is (= "a\nb" (panes/last-assistant-text
                 [(assistant [{:type "thinking" :thinking "t"} {:type "text" :text "a"}
                              {:type "tool_use"} {:type "text" :text "b"}])])))
  (is (= "older" (panes/last-assistant-text [(assistant "older") (assistant [{:type "tool_use"}])]))
      "a message with no text falls back to an older one")
  (is (= "older" (panes/last-assistant-text
                  [(assistant "older") (json/write-str {:type "user" :message {:content "\"assistant\""}})]))
      "a user line that mentions assistant is not one")
  (is (nil? (panes/last-assistant-text [])))
  (is (nil? (panes/last-assistant-text ["not json \"assistant\""]))))

(deftest compact-boundaries-are-exact
  (let [line (fn [m] (json/write-str (merge {:type "system" :subtype "compact_boundary"} m)))]
    (is (= [{:pre-tokens 100 :post-tokens 10 :trigger "manual"}
            {:pre-tokens nil :post-tokens nil :trigger nil}]
           (panes/compact-boundaries
            [(line {:compactMetadata {:preTokens 100 :postTokens 10 :trigger "manual"}})
             (json/write-str {:type "user" :message {:content "compact_boundary"}})
             (json/write-str {:type "system" :subtype "other" :x "compact_boundary"})
             (line {})])))))
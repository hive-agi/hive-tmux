(ns hive-tmux.claude.panes-project-test
  "Every `panes` row carries :project, the hive project id of the pane's cwd,
   resolved by the same resolver pane senses use (host/project-of), once per
   distinct cwd per call. Unknown cwd adds no key."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-tmux.claude.host :as host]
            [hive-tmux.claude.panes :as panes]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private list-out
  (str "%1\t1\t1\t1\t100\tclaude\t/w/funeraria\tfun-a\n"
       "%2\t1\t1\t2\t101\tclaude\t/w/hive/hive-agent\thive-b\n"
       "%3\t1\t1\t3\t102\tclaude\t/tmp\tscratch\n"))

(defn- stub-host
  ([project-of calls] (stub-host project-of calls list-out))
  ([project-of calls out]
   (cond-> {:tmux!      (fn [argv & _]
                          (if (= "list-panes" (first argv))
                            {:exit 0 :out out :err ""}
                            {:exit 0 :out "" :err ""}))
            :sessions   (constantly [])
            :transcript (constantly nil)
            :sleep!     (fn [_])
            :now-ms     (constantly 0)}
     project-of (assoc :project-of (fn [cwd] (swap! calls conj cwd) (project-of cwd))))))

(deftest rows-carry-the-project-of-their-cwd
  (let [calls (atom [])
        resolver {"/w/funeraria" "funeraria" "/w/hive/hive-agent" "hive-agent"}
        rows (panes/panes (stub-host resolver calls) {})]
    (is (= [["%1" "funeraria"] ["%2" "hive-agent"] ["%3" nil]]
           (mapv (juxt :pane :project) rows)))
    (is (not (contains? (last rows) :project)) "an unresolved cwd adds no :project key")))

(deftest each-cwd-is-resolved-once-per-call
  (let [calls (atom [])
        out (str list-out "%4\t1\t1\t4\t103\tclaude\t/w/funeraria\tfun-c\n")
        resolver {"/w/funeraria" "funeraria" "/w/hive/hive-agent" "hive-agent"}]
    (panes/panes (stub-host resolver calls out) {})
    (is (= 1 (get (frequencies @calls) "/w/funeraria"))
        "two panes in one project resolve that cwd once")))

(deftest a-host-without-project-of-adds-nothing
  (let [rows (panes/panes (stub-host nil nil) {})]
    (is (= 3 (count rows)))
    (is (not-any? #(contains? % :project) rows))))

(deftest project-of-treats-global-blank-and-throws-as-none
  (testing "global means no project"
    (is (nil? (host/project-of (constantly "global") "/x"))))
  (testing "a blank answer is no project"
    (is (nil? (host/project-of (constantly "") "/x"))))
  (testing "a resolver that throws is no project"
    (is (nil? (host/project-of (fn [_] (throw (Exception. "boom"))) "/x"))))
  (testing "a blank or nil cwd is never resolved"
    (is (nil? (host/project-of (constantly "p") "")))
    (is (nil? (host/project-of (constantly "p") nil))))
  (testing "no resolver loaded is no project"
    (is (nil? (host/project-of nil "/x"))))
  (testing "a real project id comes through"
    (is (= "hive-tmux" (host/project-of (constantly "hive-tmux") "/x")))))

(deftest cli-host-exposes-project-of
  (is (fn? (:project-of (host/cli-host {})))))

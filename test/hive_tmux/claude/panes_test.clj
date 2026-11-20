(ns hive-tmux.claude.panes-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-tmux.claude.panes :as panes]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private session
  {:pid "200" :sessionId "sid-1" :name "caveman workflow catchup"
   :cwd "/srv/peer" :status "idle" :updatedAt 1})

(def ^:private title-fixtures
  ["caveman workflow catchup"
   "✳ caveman workflow catchup"
   "✶ caveman workflow catchup"
   "⠋ caveman workflow catchup"
   "◉ caveman workflow catchup"
   "◉ xhigh · /effort  caveman workflow catchup"
   "✶ high · /effort  caveman workflow catchup"])

(defn- fixture-host [title]
  {:tmux! (fn [argv]
            (case (first argv)
              "list-panes" {:exit 0 :out (str "%3\t1\t3\t1\t100\tclaude\t/srv/peer\t"
                                             title "\t/dev/pts/7\n")}
              "capture-pane" {:exit 0 :out (str/join "\n" ["✻ Worked for 3m 12s"
                                                              "   41250 tokens"
                                                              "❯ "])}))
   :sessions (constantly [session])})

(deftest title-variants-join-through-a-screen-without-a-footer
  (doseq [title title-fixtures]
    (let [row (first (panes/panes (fixture-host title)))]
      (is (= ["%3" "caveman workflow catchup" "sid-1" :idle 41250]
             ((juxt :pane :name :session-id :status :ctx-tokens) row))
          title)
      (is (false? (:busy? row)) title)
      (is (not (:unjoined? row)) title)
      (is (= "/dev/pts/7" (:tty row)) title))))

(deftest a-stale-footer-does-not-hide-the-title-match
  (let [row (panes/pane-row {:title "◉ xhigh · /effort  caveman workflow catchup"
                             :cwd "/srv/peer"}
                            {:footer-name "stale pane name" :ctx-tokens 41250}
                            [session])]
    (is (= "caveman workflow catchup" (:name row)))
    (is (= "sid-1" (:session-id row)))))

(deftest exact-terminal-identity-is-the-last-resort
  (testing "tty"
    (let [row (panes/pane-row {:title "unrelated" :tty "/dev/pts/7"} {}
                              [(assoc session :tty "/dev/pts/7")])]
      (is (= "sid-1" (:session-id row)))))
  (testing "a numeric session pid equals the tmux string pid"
    (let [row (panes/pane-row {:title "unrelated" :pid "1234"} {}
                              [(assoc session :pid 1234)])]
      (is (= "sid-1" (:session-id row)))))
  (testing "blank and missing identities never match absent session identities"
    (doseq [pane [{:title "unrelated"}
                  {:title "unrelated" :tty "" :pid ""}
                  {:title "unrelated" :tty nil :pid nil}]]
      (let [row (panes/pane-row pane {} [(assoc session :name "other" :pid nil)])]
        (is (true? (:unjoined? row)) (pr-str pane)))))
  (testing "a name match is authoritative"
    (let [row (panes/pane-row {:title "✳ caveman workflow catchup" :cwd "/srv/peer"
                               :pid "200"}
                              {}
                              [(assoc session :pid "other")
                               (assoc session :name "other" :sessionId "other" :pid "200")])]
      (is (= "sid-1" (:session-id row))))))

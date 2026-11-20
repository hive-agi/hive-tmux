(ns hive-tmux.claude.panes-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-tmux.claude.panes :as panes]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private session
  {:pid 200 :sessionId "sid-1" :name "caveman workflow catchup"
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

(deftest a-coincidental-pid-never-joins-a-session
  (let [row (panes/pane-row {:pane "%1" :pid "25078" :tty "/dev/pts/4"
                             :title "◉ xhigh · /effort  gamma"}
                            {}
                            [{:pid 25078 :sessionId "sid-STALE" :name "beta"
                              :status "busy" :pidDomain "linux:other"}])]
    (is (true? (:unjoined? row)))
    (is (nil? (:session-id row)))
    (is (= "gamma" (:name row)))
    (is (= "/dev/pts/4" (:tty row)))))

(ns hive-tmux.observe.lifecycle-test
  "Ingestion policy: what runs by default, and the one part that touches the
   operator's tmux server never running unless asked."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.observe.lifecycle :as lc]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(deftest hook-installation-is-off-by-default
  (testing "set-hook -g is a persistent change to a server hive does not own"
    (is (false? (lc/install-hooks? {})))
    (is (false? (lc/install-hooks? {:tmux/observe? true})))
    (is (true? (lc/install-hooks? {:tmux/observe-install-hooks? true})))))

(deftest ingestion-is-on-by-default-because-the-listener-is-inert
  (is (true? (lc/enabled? {})))
  (is (false? (lc/enabled? {:tmux/observe? false}))))

(deftest defaults-and-overrides
  (testing "the default socket lives under the per-user runtime dir"
    (let [p (lc/default-socket-path)]
      (is (.endsWith p "hive-tmux-observe.sock"))
      (is (< (count p) 100) "must stay under the OS sun_path cap")))
  (testing "operator config wins over every default"
    (let [c (lc/config->observe {:tmux/observe-socket "/run/x.sock"
                                 :tmux/observe-hooks ["pane-died"]
                                 :tmux/observe-client-cmd ["cljw" "hook.cljc"]})]
      (is (= "/run/x.sock" (:socket-path c)))
      (is (= ["pane-died"] (:hooks c)))
      (is (= ["cljw" "hook.cljc"] (:client-cmd c))))))

(deftest disabled-start-does-nothing
  (let [ran (atom [])]
    (is (nil? (lc/start! {:tmux/observe? false} #(swap! ran conj %))))
    (is (= [] @ran))))

(deftest start-without-opt-in-never-touches-tmux
  (let [ran (atom [])
        sock (str (System/getProperty "java.io.tmpdir") "/hive-obs-lc-" (System/nanoTime) ".sock")
        h (lc/start! {:tmux/observe-socket sock} #(swap! ran conj %))]
    (try
      (testing "the listener is up but no tmux invocation was made"
        (is (some? (:listener h)))
        (is (false? (:hooks-installed? h)))
        (is (= [] @ran)))
      (finally (lc/stop! h #(swap! ran conj %))))
    (testing "stop does not unset hooks it never installed"
      (is (= [] @ran) "unsetting an operator's own hook would destroy their config"))))

(deftest opt-in-installs-and-removes-exactly-its-own-hooks
  (let [ran (atom [])
        sock (str (System/getProperty "java.io.tmpdir") "/hive-obs-lc-" (System/nanoTime) ".sock")
        cfg {:tmux/observe-socket sock
             :tmux/observe-install-hooks? true
             :tmux/observe-hooks ["pane-died" "client-attached"]}
        h (lc/start! cfg #(swap! ran conj %))]
    (testing "one set-hook -g per configured hook"
      (is (= 2 (count @ran)))
      (is (every? #(= ["set-hook" "-g"] (subvec % 0 2)) @ran)))
    (reset! ran [])
    (lc/stop! h #(swap! ran conj %))
    (testing "and one unset per hook on the way out"
      (is (= 2 (count @ran)))
      (is (every? #(= ["set-hook" "-gu"] (subvec % 0 2)) @ran)))))

(deftest client-cmd-resolves-to-something-tmux-can-run
  (testing "a resolved binary is absolute: tmux runs hooks from a cwd we do not own"
    (let [cmd (lc/default-client-cmd)]
      (is (vector? cmd))
      (is (seq cmd))
      (is (every? string? cmd))
      (if (= 1 (count cmd))
        (let [bin (first cmd)]
          (is (.isAbsolute (java.io.File. ^String bin))
              "a relative binary path would resolve against tmux's cwd")
          (is (.canExecute (java.io.File. ^String bin))))
        (is (= ["cljw" "-cp" "src:native" "-m" "hive-tmux.observe.hook-client"] cmd)
            "the only non-binary fallback is running the source through cljw")))))

(ns hive-tmux.observe.lifecycle-test
  "Ingestion policy: what runs by default, and the one part that touches the
   operator's tmux server never running unless asked."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.observe.lifecycle :as lc]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- tmp-sock
  "A socket path in a fresh directory this process owns (the adapter
   refuses a socket directory owned by another user, e.g. /tmp itself)."
  []
  (str (java.nio.file.Files/createTempDirectory
        "hive-obs-lc-" (make-array java.nio.file.attribute.FileAttribute 0))
       "/tmux-observe.sock"))

(deftest hook-installation-is-off-by-default
  (testing "set-hook -g is a persistent change to a server hive does not own"
    (is (false? (lc/install-hooks? {})))
    (is (false? (lc/install-hooks? {:tmux/observe? true})))
    (is (true? (lc/install-hooks? {:tmux/observe-install-hooks? true})))))

(deftest ingestion-is-on-by-default-because-the-listener-is-inert
  (is (true? (lc/enabled? {})))
  (is (false? (lc/enabled? {:tmux/observe? false}))))

(deftest defaults-and-overrides
  (testing "the default socket is the STD-04 T2 path of service tmux-observe"
    (let [p   (lc/default-socket-path)
          xdg (System/getenv "XDG_RUNTIME_DIR")]
      (is (.endsWith p "/tmux-observe.sock"))
      (if (seq xdg)
        (is (.startsWith p (str xdg "/hive/")))
        (is (re-find #"/hive-\d+/tmux-observe\.sock$" p)))
      (is (<= (alength (.getBytes p "UTF-8")) 100) "must stay under the OS sun_path cap")))
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
        sock (tmp-sock)
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
        sock (tmp-sock)
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

(deftest a-second-instance-on-a-live-path-leaves-the-hooks-alone
  (let [sock  (tmp-sock)
        ran   (atom [])
        cfg   {:tmux/observe-socket sock
               :tmux/observe-install-hooks? true
               :tmux/observe-hooks ["pane-died"]}
        live  (lc/start! cfg (fn [_]))]
    (try
      (is (some? (:listener live)) "precondition: the first instance is live")
      (let [h2 (lc/start! cfg #(swap! ran conj %))]
        (testing "the second start does not bind and installs no hook"
          (is (nil? (:listener h2)))
          (is (false? (:hooks-installed? h2)))
          (is (= [] @ran)))
        (lc/stop! h2 #(swap! ran conj %))
        (testing "and its stop! unsets nothing the live instance installed"
          (is (= [] @ran))))
      (finally (lc/stop! live (fn [_]))))))

(deftest installed-hooks-carry-the-listener-path-explicitly
  (testing "a non-default socket path reaches every hook payload as given, never as `-`"
    (let [sock (tmp-sock)
          ran  (atom [])
          h    (lc/start! {:tmux/observe-socket sock
                           :tmux/observe-install-hooks? true
                           :tmux/observe-hooks ["pane-died" "client-attached"]}
                          #(swap! ran conj %))]
      (try
        (is (= 2 (count @ran)))
        (doseq [argv @ran]
          (is (.contains ^String (last argv) (str " " sock " ")))
          (is (not (.contains ^String (last argv) " - "))))
        (finally (lc/stop! h (fn [_]))))))
  (testing "the default path is resolved to a concrete absolute path before planning"
    (is (.startsWith ^String (:socket-path (lc/config->observe {})) "/"))))

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

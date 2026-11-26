(ns hive-tmux.observe.socket-path-test
  "The hook client's copy of the STD-04 T2 rule (hive-tmux.observe.socket-path)
   against the reference the JVM listener uses (hive-system.network.path):
   shared vectors, generated inputs, and the listener default itself."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-system.network.path :as ref]
            [hive-system.network.unix :as unix]
            [hive-tmux.observe.lifecycle :as lc]
            [hive-tmux.observe.socket-path :as sp]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def vectors
  (edn/read-string (slurp (io/file "test/hive_tmux/observe/socket_path_vectors.edn"))))

(defn- outcome
  "The :path of an ok result, else its :error keyword."
  [res]
  (if (contains? res :ok) (:path (:ok res)) (:error res)))

(deftest both-copies-meet-the-shared-vectors
  (is (seq vectors))
  (doseq [{:keys [in out]} vectors]
    (testing (pr-str in)
      (is (= out (outcome (apply sp/socket-path in))) "hook-client copy")
      (is (= out (outcome (apply ref/socket-path in))) "hive-system reference"))))

(def gen-text
  (gen/one-of [(gen/return nil) (gen/return "") (gen/return "/") gen/string
               (gen/fmap #(str "/" %) gen/string-alphanumeric)
               (gen/fmap #(str "/" % "/") gen/string)]))

(defspec copies-agree-on-generated-inputs 300
  (prop/for-all [xdg    gen-text
                 tmpdir gen-text
                 uid    (gen/one-of [(gen/return nil) gen/nat])
                 svc    (gen/one-of [(gen/return sp/service) gen/string-alphanumeric gen/string])
                 id     (gen/one-of [(gen/return nil) gen/string-alphanumeric gen/string])]
    (let [env (if xdg {"XDG_RUNTIME_DIR" xdg} {})]
      (= (apply sp/socket-path [env tmpdir uid svc id])
         (apply ref/socket-path [env tmpdir uid svc id])))))

(deftest hook-client-path-equals-listener-path
  (let [tmpdir (System/getProperty "java.io.tmpdir")
        uid    (unix/current-uid)
        client #(:path (:ok (sp/socket-path % tmpdir uid sp/service)))
        listen #(:path (:ok (ref/socket-path % tmpdir uid "tmux-observe")))]
    (testing "with XDG_RUNTIME_DIR set"
      (let [env {"XDG_RUNTIME_DIR" "/run/user/4242"}]
        (is (= "/run/user/4242/hive/tmux-observe.sock" (client env)))
        (is (= (listen env) (client env)))))
    (testing "with XDG_RUNTIME_DIR unset"
      (is (some? uid) "the JDK names the uid on this host")
      (is (= (str tmpdir "/hive-" uid "/tmux-observe.sock") (client {})))
      (is (= (listen {}) (client {}))))
    (testing "the listener's actual default is what the client computes from this environment"
      (is (= (lc/default-socket-path)
             (client {"XDG_RUNTIME_DIR" (System/getenv "XDG_RUNTIME_DIR")}))))))

(deftest client-uid-source-matches-the-jdk
  (testing "the hook client reads the uid from /proc/self/status; it is the JDK's uid"
    (when (.exists (io/file "/proc/self/status"))
      ;; NIO, not slurp: on the JVM procfs refuses available() with EINVAL
      (is (= (unix/current-uid)
             (sp/proc-status-uid
              (String. (java.nio.file.Files/readAllBytes
                        (.toPath (io/file "/proc/self/status")))
                       "UTF-8"))))))
  (is (nil? (sp/proc-status-uid nil)))
  (is (= 1000 (sp/proc-status-uid "Name:\tx\nUid:\t1000\t1000\t1000\t1000\nGid:\t1"))))

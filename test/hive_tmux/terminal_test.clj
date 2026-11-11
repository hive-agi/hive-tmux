(ns hive-tmux.terminal-test
  "e2e tests: they drive a real tmux through libtmux, so they are tagged
   ^:e2e and the default run (tests.edn) skips them.

   Each test runs on a PRIVATE tmux server (`tmux -L hive-e2e-<uuid>`) that the
   fixture creates and kills. The socket is bound before any tmux call and the
   fixture refuses to run without it, so even a run that forgets the skip flag
   can never touch the server an operator runs.

   Requires tmux and libtmux (conda hive env).
   Run: clojure -M:test --focus-meta :e2e"
  (:require [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-tmux.python.bridge :as py-bridge]
            [hive-tmux.python.tmux-ops :as tmux-ops]
            [hive-tmux.state :as state]))

(defn private-tmux
  "Run F with every tmux-ops call bound to a fresh private socket, then kill
   that server. The live default server is never addressed."
  [f]
  (let [sock (str "hive-e2e-" (random-uuid))]
    (assert (re-matches #"hive-e2e-[0-9a-f-]+" sock))
    (binding [tmux-ops/*socket-name* sock]
      (try
        (shell/sh "tmux" "-L" sock "new-session" "-d" "-s" "hive")
        (tmux-ops/reset-server!)
        (f)
        (finally
          (tmux-ops/reset-server!)
          (shell/sh "tmux" "-L" sock "kill-server"))))))

(use-fixtures :each private-tmux)

(deftest ^:e2e pane-lifecycle-test
  (testing "create, send-keys, capture, kill pane lifecycle"
    (is (some? tmux-ops/*socket-name*) "never the default server")
    (is (py-bridge/ensure-python!) "libtmux is needed for this e2e test")
    (let [pane-info (tmux-ops/create-pane! {:ling-id "test-ling" :cwd "/tmp"})
          pane (:pane pane-info)]
      (is (some? (:pane-id pane-info)))
      (is (some? (:window-id pane-info)))
      (state/register-pane! "test-ling" pane-info {:cwd "/tmp"})
      (is (state/registered? "test-ling"))
      (tmux-ops/send-keys! pane "echo hello-from-test")
      (Thread/sleep 500)
      (is (vector? (tmux-ops/capture-output pane {:lines 10})))
      (is (tmux-ops/pane-alive? (:pane-id pane-info)))
      (tmux-ops/kill-pane! pane)
      (state/deregister-pane! "test-ling")
      (Thread/sleep 200)
      (is (not (tmux-ops/pane-alive? (:pane-id pane-info))))
      (is (not (state/registered? "test-ling"))))))

(deftest ^:e2e interrupt-test
  (testing "interrupt sends C-c to pane"
    (is (some? tmux-ops/*socket-name*) "never the default server")
    (is (py-bridge/ensure-python!) "libtmux is needed for this e2e test")
    (let [pane (:pane (tmux-ops/create-pane! {:ling-id "test-interrupt" :cwd "/tmp"}))]
      (tmux-ops/send-keys! pane "sleep 300")
      (Thread/sleep 200)
      (tmux-ops/send-interrupt! pane)
      (Thread/sleep 500)
      (tmux-ops/kill-pane! pane))))

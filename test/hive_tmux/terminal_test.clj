(ns hive-tmux.terminal-test
  "Integration tests for tmux terminal addon.

   Requires:
   - tmux running with a session
   - libtmux installed in conda hive env
   - hive-mcp protocols on classpath

   Run: clojure -M:test -m kaocha.runner --focus :integration"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-tmux.python.bridge :as py-bridge]
            [hive-tmux.python.tmux-ops :as tmux-ops]
            [hive-tmux.state :as state]))

;; =============================================================================
;; Integration tests — require running tmux + libtmux
;; =============================================================================

(deftest ^:integration pane-lifecycle-test
  (testing "create, send-keys, capture, kill pane lifecycle"
    (when (py-bridge/ensure-python!)
      (let [pane-info (tmux-ops/create-pane! {:ling-id "test-ling"
                                              :cwd "/tmp"})
            pane (:pane pane-info)]
        (is (some? (:pane-id pane-info)))
        (is (some? (:window-id pane-info)))

        ;; Register in state
        (state/register-pane! "test-ling" pane-info {:cwd "/tmp"})
        (is (state/registered? "test-ling"))

        ;; Send a command
        (tmux-ops/send-keys! pane "echo hello-from-test")
        (Thread/sleep 500)

        ;; Capture output
        (let [output (tmux-ops/capture-output pane {:lines 10})]
          (is (vector? output)))

        ;; Pane should be alive
        (is (tmux-ops/pane-alive? (:pane-id pane-info)))

        ;; Kill pane
        (tmux-ops/kill-pane! pane)
        (state/deregister-pane! "test-ling")

        ;; Pane should be dead
        (Thread/sleep 200)
        (is (not (tmux-ops/pane-alive? (:pane-id pane-info))))
        (is (not (state/registered? "test-ling")))))))

(deftest ^:integration interrupt-test
  (testing "interrupt sends C-c to pane"
    (when (py-bridge/ensure-python!)
      (let [pane-info (tmux-ops/create-pane! {:ling-id "test-interrupt"
                                              :cwd "/tmp"})
            pane (:pane pane-info)]
        ;; Start a long-running command
        (tmux-ops/send-keys! pane "sleep 300")
        (Thread/sleep 200)

        ;; Interrupt it
        (tmux-ops/send-interrupt! pane)
        (Thread/sleep 500)

        ;; Clean up
        (tmux-ops/kill-pane! pane)))))

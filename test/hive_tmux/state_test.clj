(ns hive-tmux.state-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-test.resource-assertions :refer [resource-cleanup-fixture]]
            [hive-tmux.state :as state]))

(use-fixtures :each
  (fn [f]
    (state/clear-registry!)
    (resource-cleanup-fixture
     (fn []
       (try (f)
            (finally (state/clear-registry!)))))))

(deftest register-pane-test
  (testing "registers a pane and can retrieve it"
    (let [pane-info {:pane-id "%1" :window-id "@1" :pane :mock-pane :window :mock-window}
          entry (state/register-pane! "ling-1" pane-info {:cwd "/tmp"})]
      (is (= "%1" (:pane-id entry)))
      (is (= "/tmp" (:cwd entry)))
      (is (= entry (state/get-pane "ling-1")))))

  (testing "overwrites existing registration"
    (state/register-pane! "ling-1"
                          {:pane-id "%1" :window-id "@1" :pane :p1 :window :w1}
                          {:cwd "/a"})
    (state/register-pane! "ling-1"
                          {:pane-id "%2" :window-id "@2" :pane :p2 :window :w2}
                          {:cwd "/b"})
    (is (= "%2" (:pane-id (state/get-pane "ling-1"))))))

(deftest deregister-pane-test
  (testing "removes a registered pane"
    (state/register-pane! "ling-1"
                          {:pane-id "%1" :window-id "@1" :pane :p :window :w}
                          {:cwd "/tmp"})
    (let [removed (state/deregister-pane! "ling-1")]
      (is (= "%1" (:pane-id removed)))
      (is (nil? (state/get-pane "ling-1")))))

  (testing "returns nil for unknown ling"
    (is (nil? (state/deregister-pane! "nonexistent")))))

(deftest registered?-test
  (testing "returns false for unknown ling"
    (is (false? (state/registered? "ling-1"))))

  (testing "returns true after registration"
    (state/register-pane! "ling-1"
                          {:pane-id "%1" :window-id "@1" :pane :p :window :w}
                          {})
    (is (true? (state/registered? "ling-1")))))

(deftest get-all-panes-test
  (testing "returns all registered panes"
    (state/register-pane! "ling-1"
                          {:pane-id "%1" :window-id "@1" :pane :p1 :window :w1}
                          {})
    (state/register-pane! "ling-2"
                          {:pane-id "%2" :window-id "@2" :pane :p2 :window :w2}
                          {})
    (let [all (state/get-all-panes)]
      (is (= 2 (count all)))
      (is (contains? all "ling-1"))
      (is (contains? all "ling-2")))))

(deftest clear-registry-test
  (testing "clears all panes"
    (state/register-pane! "ling-1"
                          {:pane-id "%1" :window-id "@1" :pane :p :window :w}
                          {})
    (state/clear-registry!)
    (is (empty? (state/get-all-panes)))))

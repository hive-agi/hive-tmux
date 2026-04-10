(ns hive-tmux.state-property-test
  "Property-based tests for the tmux pane registry."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-test.properties :as props]
            [hive-test.generators.core :as gen-core]
            [hive-test.resource-assertions :refer [resource-cleanup-fixture]]
            [hive-tmux.state :as state]
            [clojure.test.check.generators :as gen]))

;; Fixture: resource leak detection + registry cleanup
(use-fixtures :each
  (fn [f]
    (state/clear-registry!)
    (resource-cleanup-fixture
     (fn []
       (try (f)
            (finally (state/clear-registry!)))))))

;; Generator: mock pane-info
(def gen-pane-info
  (gen/let [pane-id (gen/fmap #(str "%" %) gen/nat)
            win-id  (gen/fmap #(str "@" %) gen/nat)]
    {:pane-id pane-id :window-id win-id :pane :mock :window :mock}))

;; Property: register then get always returns non-nil (totality)
(props/defprop-total state-register-total
  (fn [ling-id]
    (state/clear-registry!)
    (state/register-pane! ling-id
                          {:pane-id "%1" :window-id "@1" :pane :mock :window :mock}
                          {:cwd "/tmp"})
    (state/get-pane ling-id))
  gen-core/gen-non-blank-string
  {:pred some?})

;; Property: clear is idempotent — clearing twice yields same result as once
(props/defprop-idempotent state-clear-idempotent
  (fn [_]
    (state/clear-registry!)
    (state/get-all-panes))
  (gen/return nil))

;; Property: invariant — after N registers, registry has at most N entries
;; (may be fewer due to duplicate ling-ids)
(deftest state-count-invariant-test
  (testing "registry count <= number of register calls with unique ids"
    (let [ids (mapv #(str "ling-" %) (range 10))]
      (state/clear-registry!)
      (doseq [id ids]
        (state/register-pane! id
                              {:pane-id (str "%" id) :window-id "@1" :pane :mock :window :mock}
                              {:cwd "/tmp"}))
      (is (= (count ids) (count (state/get-all-panes)))))))

;; Property: deregister after register returns the entry
(props/defprop-total state-deregister-returns-entry
  (fn [ling-id]
    (state/clear-registry!)
    (let [info {:pane-id "%1" :window-id "@1" :pane :mock :window :mock}]
      (state/register-pane! ling-id info {:cwd "/tmp"})
      (state/deregister-pane! ling-id)))
  gen-core/gen-non-blank-string
  {:pred some?})

;; Property: after deregister, get returns nil
(props/defprop-total state-deregister-then-get-nil
  (fn [ling-id]
    (state/clear-registry!)
    (state/register-pane! ling-id
                          {:pane-id "%1" :window-id "@1" :pane :mock :window :mock}
                          {:cwd "/tmp"})
    (state/deregister-pane! ling-id)
    (state/get-pane ling-id))
  gen-core/gen-non-blank-string
  {:pred nil?})

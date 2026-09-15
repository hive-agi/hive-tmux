(ns hive-tmux.observe.hook-plan-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-schemas.test :as hst]
            [hive-tmux.observe.hook-plan :as plan]
            [hive-tmux.observe.schema :as schema]))

(def config
  {:socket-path "/run/hive/tmux-observe.sock"
   :client-cmd ["cljw" "-cp" "src" "-M" "native/hive_tmux_hook.cljc"]
   :hooks (vec plan/default-hooks)})

;; Schema-generated: every generated config must plan one argv per hook.
(hst/deftrifecta-from-schema install-plan
  hive-tmux.observe.hook-plan/install-plan
  {:in schema/ObserveConfig
   :out [:vector schema/HookCommand]
   :rel (fn [cfg out] (= (count (:hooks cfg)) (count out)))
   :mutation true
   :num-tests 60})

(hst/deftrifecta-from-schema uninstall-plan
  hive-tmux.observe.hook-plan/uninstall-plan
  {:in schema/ObserveConfig
   :out [:vector schema/HookCommand]
   :rel (fn [cfg out] (= (count (:hooks cfg)) (count out)))
   :mutation true
   :num-tests 60})

(deftest install-plan-shape
  (testing "each hook is set globally"
    (doseq [argv (plan/install-plan config)]
      (is (= "set-hook" (first argv)))
      (is (= "-g" (second argv)))))
  (testing "the payload carries socket, hook and the tmux pane format"
    (let [argv (first (plan/install-plan config))
          payload (last argv)]
      (is (str/includes? payload (:socket-path config)))
      (is (str/includes? payload "pane-died"))
      (is (str/includes? payload "#{hook_pane}")
          "tmux substitutes the firing pane; without it every event is paneless"))))

(deftest uninstall-unsets-rather-than-blanks
  (testing "-u removes the hook; an empty hook would still run"
    (doseq [argv (plan/uninstall-plan config)]
      (is (= ["set-hook" "-gu"] (subvec argv 0 2)))
      (is (= 3 (count argv)) "no payload on an unset"))))

(deftest default-hooks-exclude-hive-caused-events
  (testing "hive already emits its own pane-created event; observing it double-counts"
    (is (not (contains? (set plan/default-hooks) "after-split-window")))
    (is (not (contains? (set plan/default-hooks) "after-new-window")))))

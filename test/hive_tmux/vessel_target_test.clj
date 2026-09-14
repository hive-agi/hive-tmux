(ns hive-tmux.vessel-target-test
  "The hive-vessel target of hive-tmux: option derivation is schema-synthesized
   (conformance + relation + mutation) with hand mutants for the two ways it
   can drift, the descriptor executes :text natives through an injected tmux
   port, the hook is contributed only while the addon is active, and the fleet
   sample batch is golden-locked for the :text dialect the way every editor
   repo locks it for its own."
  (:require [clojure.test :refer [deftest is]]
            [hive-addon.protocol :as addon]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.mutation :as mut]
            [hive-tmux.init :as init]
            [hive-tmux.vessel-target :as vt]
            [hive-vessel.core :as v]
            [hive-vessel.doc :as d]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;;; target-opts: the pure Promote step

(hst/deftrifecta-from-schema target-opts
  hive-tmux.vessel-target/target-opts
  {:in vt/Config
   :out vt/TargetOpts
   :rel (fn [config opts]
          (and (= (or (:tmux/session config) vt/default-session) (:session opts))
               (= (:tmux/socket-name config) (:socket-name opts))
               (= (:tmux/binary config) (:tmux opts))
               (= (:tmux/editor config) (:editor opts))
               (= (:vessel/features config) (:features opts))))
   :mutation true
   :num-tests 100})

(mut/deftest-mutations target-opts-defaults-the-session-and-drops-unset-keys
  hive-tmux.vessel-target/target-opts
  [["forgets-the-default-session"
    (fn [config]
      (into {} (remove (comp nil? val))
            {:session (:tmux/session config)
             :socket-name (:tmux/socket-name config)
             :tmux (:tmux/binary config)
             :editor (:tmux/editor config)
             :features (:vessel/features config)}))]
   ["keeps-unset-keys-as-nil"
    (fn [config]
      {:session (or (:tmux/session config) vt/default-session)
       :socket-name (:tmux/socket-name config)
       :tmux (:tmux/binary config)
       :editor (:tmux/editor config)
       :features (:vessel/features config)})]]
  (fn []
    (is (= {:session "hive"} (vt/target-opts {})))
    (is (= {:session "hive"} (vt/target-opts {:tmux/binary nil})))
    (is (= {:session "s" :tmux "tmux-3.4" :features #{:carto-flow/pane}}
           (vt/target-opts {:tmux/session "s" :tmux/binary "tmux-3.4"
                            :vessel/features #{:carto-flow/pane}})))))

;;; target: the descriptor, executing through the injected port

(defn- recording-port
  "A tmux port that records every argv and answers like tmux would."
  []
  (let [log (atom [])]
    [(fn [argv]
       (swap! log conj argv)
       (if (= "new-window" (first argv))
         {:exit 0 :out "@7\n" :err ""}
         {:exit 0 :out "" :err ""}))
     log]))

(deftest the-target-is-a-text-vessel-executing-through-the-tmux-port
  (let [[run! log] (recording-port)
        target (vt/target {:tmux/session "hive-test"} {:run! run!})
        r (v/dispatch! (v/standard-registry) target
                       [{:op :ui/notify :message "frame 7 applied"}
                        {:op :ui/send-to-terminal :terminal "%3" :text "ls\n"}])]
    (is (= {:vessel/id :tmux :vessel/dialect :text}
           (select-keys target [:vessel/id :vessel/dialect])))
    (is (fn? (:vessel/execute! target)))
    (is (:ok r) (pr-str r))
    (is (= [["display-message" "-t" "hive-test" "[info] frame 7 applied"]
            ["send-keys" "-t" "%3" "-l" "ls"]
            ["send-keys" "-t" "%3" "Enter"]]
           @log)
        "the session from the addon config is the one the executor targets")))

(deftest features-from-the-config-are-advertised-on-the-descriptor
  (is (= #{:carto-flow/pane}
         (:vessel/features (vt/target {:vessel/features #{:carto-flow/pane}}))))
  (is (not (contains? (vt/target {}) :vessel/features))))

(deftest only-text-natives-are-accepted
  (let [[run! log] (recording-port)
        execute! (:vessel/execute! (vt/target {} {:run! run!}))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (execute! {:op :vessel/native :native/dialect :elisp
                            :native/payload "(ignore)"})))
    (is (empty? @log) "nothing reached tmux")))

;;; hooks-for: the addon-facing projection

(deftest the-hook-is-contributed-only-while-initialized-and-resolves-per-call
  (let [state (atom {:initialized? false})]
    (is (= {} (vt/hooks-for state)))
    (reset! state {:initialized? true :config {:tmux/session "s"}})
    (let [hook (get (vt/hooks-for state) vt/target-hook-key)]
      (is (fn? hook))
      (is (= {:vessel/id :tmux :vessel/dialect :text}
             (select-keys (hook) [:vessel/id :vessel/dialect])))
      (reset! state {:initialized? false})
      (is (nil? (hook))
          "a hook kept across shutdown answers nil, not a target for a server nobody drives"))))

(deftest an-addon-that-is-not-active-contributes-no-hooks
  (let [a (init/addon-ctor {})]
    (is (some? a) "the IAddon protocol is on the classpath")
    (is (= {} (addon/hooks a)))
    (is (contains? (addon/capabilities a) :vessel))))

;;; the fleet sample batch, golden-locked for :text

(def sample-doc
  (d/doc "Carto \"Flow\" \\ #3"
         (d/heading "apply write-form")
         (d/para "succeeded" :success)
         (d/fields [["paths" "src/a.clj\nsrc/b.clj"] ["verify" "ok"]])
         (d/items ["one" "two\nmore"])
         (d/code "(defn f [] \"x\")" "clojure")
         (d/diff "@@ -1,2 +1,2 @@\n-(old)\n+(new ü)\n context")
         (d/link "open a" "/tmp/a.clj" 2)))

(def sample-batch
  [{:op :ui/show-panel :panel/id "olympus/tab-2" :doc sample-doc}
   {:op :ui/notify :message "frame 7 applied"}
   {:op :ui/notify :message "boom\nsecond line" :level :error}
   {:op :ui/open-file :file "/tmp/a b.clj" :line 3 :column 7}
   {:op :ui/open-file :file "/tmp/a.clj"}
   {:op :ui/send-to-terminal :terminal "*vterm*" :text "ls -la\n"}
   {:op :ui/close-panel :panel/id "olympus/tab-2"}])

(defn- natives []
  (let [target (dissoc (vt/target) :vessel/execute!)
        r (v/plan (v/standard-registry) target sample-batch)]
    (is (:ok r) (pr-str (:error r)))
    (mapv :native/payload (get-in r [:ok :plan/ops]))))

(deftest every-sample-op-plans-for-tmux
  (is (= (count sample-batch) (count (natives))))
  (is (every? #(contains? % :text/lines) (natives))
      "every native is a :text payload with its lines"))

(deftest-golden the-sample-batch-lowers-to-these-text-payloads
  "test/golden/vessel/sample-batch-text.edn"
  (natives))

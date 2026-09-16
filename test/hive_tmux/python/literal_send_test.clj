(ns hive-tmux.python.literal-send-test
  (:require [clojure.test :refer [deftest is]]
            [hive-tmux.python.bridge :as py]
            [hive-tmux.python.tmux-ops :as ops]))

(deftest text-cannot-become-terminal-key-names
  (let [calls (atom [])
        adapter {:call (fn [& args] (swap! calls conj (vec args)) nil)
                 :attr (fn [& _] "%test")}]
    (ops/send-keys! :pane "C-c" {} adapter)
    (ops/send-keys! :pane "Enter" {:enter? false} adapter)
    (is (= [[:pane "send_keys" "C-c" :enter true :literal true]
            [:pane "send_keys" "Enter" :enter false :literal true]] @calls))))

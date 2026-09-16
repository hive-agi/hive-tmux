(ns hive-tmux.python.bracketed-paste-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [hive-tmux.python.tmux-ops :as ops]))

(defn fixture [fail-command]
  (let [calls (atom []) content (atom nil) path (atom nil)]
    {:calls calls :content content :path path
     :adapter
     {:attr (fn [obj key]
              (case key "server" :server "returncode" (:exit obj) nil))
      :call (fn [obj method & args]
              (swap! calls conj (into [obj method] args))
              (when (= "load-buffer" (first args))
                (reset! path (last args))
                (reset! content (slurp @path :encoding "UTF-8")))
              {:exit (if (= fail-command (first args)) 1 0)})}}))

(deftest one-paste-frame-preserves-multiline-utf8-with-one-enter
  (let [{:keys [calls content path adapter]} (fixture nil)
        input (str "first\nsecond\r\n" (apply str (repeat 65536 "中")))]
    (ops/send-bracketed-paste! :pane input {} adapter)
    (is (= (str "\u001b[200~" input "\u001b[201~") @content))
    (is (= ["load-buffer" "paste-buffer" "send-keys" "delete-buffer"]
           (mapv #(nth % 2) @calls)))
    (let [buffer (nth (first @calls) 4)]
      (is (re-matches #"hive-self-steer-[0-9a-f-]+" buffer))
      (is (= [:pane "cmd" "paste-buffer" "-r" "-d" "-b" buffer] (second @calls)))
      (is (= [:pane "cmd" "send-keys" "Enter"] (nth @calls 2)))
      (is (= [:server "cmd" "delete-buffer" "-b" buffer] (last @calls))))
    (is (not (.exists (io/file @path))))
    ;; Large text is file content, never a process argument.
    (is (every? #(< (count (str %)) 200) (mapcat identity @calls)))))

(deftest failure-never-submits-enter-and-always-cleans-owned-resources
  (doseq [failed ["load-buffer" "paste-buffer"]]
    (let [{:keys [calls path adapter]} (fixture failed)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"tmux paste command failed"
                           (ops/send-bracketed-paste! :pane "one\ntwo" {} adapter)))
      (is (not-any? #(= "send-keys" (nth % 2)) @calls))
      (is (= "delete-buffer" (nth (last @calls) 2)))
      (is (not (.exists (io/file @path)))))))

(deftest enter-failure-cleans-up-without-retrying
  (let [{:keys [calls path adapter]} (fixture "send-keys")]
    (is (thrown? clojure.lang.ExceptionInfo
                 (ops/send-bracketed-paste! :pane "one\ntwo" {} adapter)))
    (is (= 1 (count (filter #(= "send-keys" (nth % 2)) @calls))))
    (is (= "delete-buffer" (nth (last @calls) 2)))
    (is (not (.exists (io/file @path))))))

(deftest input-cannot-break-out-of-bracketed-frame
  (doseq [text ["\u001b[200~" "\u001b[201~" "bad\u0000input" "bad\u0003input"]]
    (let [{:keys [calls adapter]} (fixture nil)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (ops/send-bracketed-paste! :pane text {} adapter)))
      (is (empty? @calls)))))

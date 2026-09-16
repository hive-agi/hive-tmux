(ns hive-tmux.python.site-packages-test
  (:require [clojure.test :refer [deftest is]]
            [hive-tmux.python.bridge :as bridge]))

(deftest embedded-package-paths-are-derived-and-idempotent
  (let [path (atom [])
        root "/configured/site-packages"
        adapter {:import identity :attr (fn [_ _] path)
                 :convert (fn [x] (if (instance? clojure.lang.IDeref x) @x x))
                 :directory? #{root}
                 :call (fn [obj method arg]
                         (case method
                           "get_path" root
                           "append" (swap! obj conj arg)))}]
    (is (true? (#'bridge/ensure-site-packages! adapter)))
    (is (= [root] @path) "purelib/platlib duplicates are added once")
    (#'bridge/ensure-site-packages! adapter)
    (is (= [root] @path) "Repeated preflight preserves sys.path")))

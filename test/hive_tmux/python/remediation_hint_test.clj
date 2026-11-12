(ns hive-tmux.python.remediation-hint-test
  "The preflight hints are read by a CUSTOMER on a fresh machine, who has no
   developer `hive` conda env. Every hint must be an instruction that works
   there, and the tmux addon must say it is optional (kanban
   20260924142831-6c1b2c13, BOOT-NOISE-CUSTOMER)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-tmux.python.bridge :as bridge]))

(def failing-statuses
  (mapv bridge/->preflight-status
        [:preflight/no-libpython :preflight/no-python-exe :preflight/python-init-fail
         :preflight/no-libtmux :preflight/no-tmux-binary]))

(deftest no-hint-names-the-developer-conda-env
  (doseq [s failing-statuses]
    (let [hint (bridge/remediation-hint s)]
      (testing (pr-str s)
        (is (string? hint))
        (is (not (re-find #"(?i)conda (create|activate)|-n hive" hint))
            hint)))))

(deftest libtmux-hint-is-a-plain-user-install
  (let [hint (bridge/remediation-hint (bridge/->preflight-status :preflight/no-libtmux))]
    (is (str/includes? hint "pip install --user libtmux") hint)
    (is (re-find #"(?i)optional" hint) hint)))

(deftest every-failing-hint-says-tmux-is-optional
  (doseq [s failing-statuses]
    (is (re-find #"(?i)optional" (bridge/remediation-hint s)) (pr-str s))))

(deftest available-has-no-hint
  (is (nil? (bridge/remediation-hint (bridge/->preflight-status :preflight/available)))))

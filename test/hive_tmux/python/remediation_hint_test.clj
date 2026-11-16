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
         :preflight/no-tmux-binary]))

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
  (doseq [s (conj failing-statuses (bridge/->preflight-status :preflight/no-libtmux))]
    (is (re-find #"(?i)optional" (bridge/remediation-hint s)) (pr-str s))))

(deftest available-has-no-hint
  (is (nil? (bridge/remediation-hint (bridge/->preflight-status :preflight/available)))))

;;; =============================================================================
;;; The pipeline the hints are about, driven through the probe port
;;; =============================================================================

(defn- run-with
  "Run the real preflight pipeline with `overrides` spliced into its probe, so
   the stage under test is the one that fails. `cached-status` remembers an
   earlier success, so reset it first."
  [overrides]
  (bridge/reset-python!)
  (bridge/run-preflight {:probe overrides}))

(deftest libtmux-install-works-on-the-target-image
  (testing "Ubuntu 24.04, the image the boot noise was seen on: no `pip` on PATH,
            and its system Python is externally managed (PEP 668), so a bare
            `pip install --user` fails with error: externally-managed-environment"
    (let [status (run-with {:libtmux-importable? (constantly false)})
          hint (bridge/remediation-hint status)]
      (is (= :preflight/no-libtmux (:adt/variant status)))
      (is (str/includes? hint "python3 -m pip install --user libtmux") hint)
      (is (str/includes? hint "--break-system-packages") hint)
      (testing "a hint that starts a line with the bare `pip` script"
        (is (not (re-find #"(?m)^\s*pip install" hint)) hint)))))

(deftest python-exe-hint-names-paths-and-the-check-reads-path
  (testing "the interpreter may live at a PATH entry that is not /usr/bin, e.g.
            pyenv, homebrew, or a venv the customer is told to put first"
    (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                        "venv-bin" (make-array java.nio.file.attribute.FileAttribute 0)))
          exe (java.io.File. dir "python3")]
      (try
        (spit exe "#!/bin/sh\n")
        (.setExecutable exe true)
        (is (= (.getAbsolutePath exe)
               (#'bridge/on-path-python (str "/nowhere:" (.getAbsolutePath dir)))))
        (is (nil? (#'bridge/on-path-python "/nowhere")))
        ;; The stub is not a real interpreter, so a later stage may fail; the
        ;; claim under test is only that stage 2 does not call it missing.
        (is (not= :preflight/no-python-exe
                  (:adt/variant (run-with {:python-exe (constantly (.getAbsolutePath exe))})))
            "a PATH-only interpreter is not reported as missing")
        (finally
          (.delete exe)
          (.delete dir)
          (bridge/reset-python!)))))
  (let [hint (bridge/remediation-hint (run-with {:python-exe (constantly nil)}))]
    (is (str/includes? hint "PATH") hint)))

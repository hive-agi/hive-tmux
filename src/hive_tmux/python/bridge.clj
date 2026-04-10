(ns hive-tmux.python.bridge
  "Low-level Python bridge utilities via libpython-clj.

   Copy-adapted from hive-agent-bridge.python.bridge — same pattern:
   lazy init, conda env detection, thin wrappers.

   No asyncio needed: libtmux operations are synchronous.

   Preflight status is a closed ADT (defadt PreflightStatus) with
   exhaustive adt-case matching for remediation hints."
  (:require [clojure.java.shell :as shell]
            [hive-dsl.adt :refer [defadt adt-case]]
            [hive-dsl.result :refer [rescue guard try-effect*]]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Lazy Python Initialization
;;; =============================================================================

(defonce ^:private python-initialized? (atom false))
(defonce ^:private cached-status (atom nil))

(def ^:private default-python-executable
  "Default Python executable path. Prefers conda 'hive' env if it exists."
  (let [conda-python "/home/lages/anaconda3/envs/hive/bin/python"]
    (if (.exists (java.io.File. conda-python))
      conda-python
      nil)))

;;; =============================================================================
;;; Preflight Status ADT (CLARITY-Y: Yield Safe Failure)
;;; =============================================================================

(defadt PreflightStatus
  "Closed set of preflight check outcomes.
   Each variant maps to a distinct failure mode with actionable remediation."
  :preflight/available
  :preflight/no-libpython
  :preflight/no-python-exe
  :preflight/python-init-fail
  :preflight/no-libtmux
  :preflight/no-tmux-binary)

;;; =============================================================================
;;; Individual Check Functions
;;; =============================================================================

(defn- check-libpython-available?
  "Check if libpython-clj2 is on the classpath."
  []
  (boolean (guard Exception false (require 'libpython-clj2.python) true)))

(defn- check-tmux-binary?
  "Check if `tmux` binary is on PATH."
  []
  (boolean
   (guard Exception false
          (let [{:keys [exit]} (shell/sh "which" "tmux")]
            (zero? exit)))))

(defn- patch-io-encoding!
  "Patch libpython-clj's JVM IO bridge to include 'encoding' attribute.
   libtmux's _compat.py reads sys.stdout.encoding at import time and crashes
   with AttributeError if missing. Must be called AFTER Python init,
   BEFORE libtmux import.

   Uses py/set-attr! (not run-simple-string) to avoid StackOverflow
   in the JVM IO bridge during string execution."
  []
  (guard Exception nil
         (let [import-fn  (requiring-resolve 'libpython-clj2.python/import-module)
               get-attr   (requiring-resolve 'libpython-clj2.python/get-attr)
               set-attr!  (requiring-resolve 'libpython-clj2.python/set-attr!)
               sys        (import-fn "sys")
               stdout     (get-attr sys "stdout")
               stderr     (get-attr sys "stderr")]
           (when-not (guard Exception false (get-attr stdout "encoding") true)
             (set-attr! stdout "encoding" "utf-8"))
           (when-not (guard Exception false (get-attr stderr "encoding") true)
             (set-attr! stderr "encoding" "utf-8")))))

(defn- check-libtmux-importable?
  "Check if libtmux Python package is importable.
   Must be called AFTER libpython-clj is initialized and IO patched."
  []
  (boolean
   (guard Exception false
          (patch-io-encoding!)
          (let [import-fn (requiring-resolve 'libpython-clj2.python/import-module)]
            (import-fn "libtmux")
            true))))

;;; =============================================================================
;;; Remediation Hints (exhaustive via adt-case)
;;; =============================================================================

(defn remediation-hint
  "Get user-facing remediation message for a preflight status ADT value.
   Exhaustive matching — compile-time safety via adt-case."
  [status]
  (adt-case PreflightStatus status
            :preflight/available     nil
            :preflight/no-libpython
            "libpython-clj2 not on classpath. Add to deps.edn:
  clj-python/libpython-clj {:mvn/version \"2.026\"}"

            :preflight/no-python-exe
            "Python executable not found. Install conda 'hive' env:
  conda create -n hive python=3.11 && conda activate hive"

            :preflight/python-init-fail
            "libpython-clj failed to initialize Python. Check:
  1. conda activate hive
  2. python --version  (should be 3.10+)
  3. Verify no conflicting Python env vars (PYTHONHOME, PYTHONPATH)"

            :preflight/no-libtmux
            "libtmux Python package not installed. Install it:
  conda activate hive && pip install libtmux"

            :preflight/no-tmux-binary
            "tmux binary not found on PATH. Install it:
  sudo apt install tmux    # Debian/Ubuntu
  sudo dnf install tmux    # Fedora
  brew install tmux        # macOS"))

;;; =============================================================================
;;; Preflight Pipeline
;;; =============================================================================

(defn run-preflight
  "Run staged preflight checks. Returns PreflightStatus ADT value.

   Check order (fail-fast):
     1. libpython-clj on classpath?
     2. Python executable exists?
     3. libpython-clj initializes?
     4. libtmux importable?
     5. tmux binary on PATH?

   Each stage fails fast with a distinct ADT variant.
   Use `remediation-hint` for user-facing install instructions."
  ([] (run-preflight {}))
  ([opts]
   (or @cached-status
       (let [status
             (cond
               ;; 1. libpython-clj on classpath?
               (not (check-libpython-available?))
               (preflight-status :preflight/no-libpython)

               ;; 2. Python executable exists?
               (let [py-exe (or (:python-executable opts) default-python-executable)]
                 (and (nil? py-exe)
                      (not (.exists (java.io.File. "/usr/bin/python3")))))
               (preflight-status :preflight/no-python-exe)

               :else
               (let [init-result
                     (try-effect* :preflight/python-init-fail
                                  (let [init! (requiring-resolve 'libpython-clj2.python/initialize!)
                                        py-exe (or (:python-executable opts) default-python-executable)]
                                    (if py-exe
                                      (init! :python-executable py-exe)
                                      (init!))))]
                 (if (:error init-result)
                   ;; 3. Python init failed
                   (preflight-status :preflight/python-init-fail)

                   ;; 4. libtmux importable?
                   (if-not (check-libtmux-importable?)
                     (preflight-status :preflight/no-libtmux)

                     ;; 5. tmux binary on PATH?
                     (if-not (check-tmux-binary?)
                       (preflight-status :preflight/no-tmux-binary)

                       ;; All checks passed
                       (preflight-status :preflight/available))))))]

         (reset! cached-status status)
         (when-not (= :preflight/available (:adt/variant status))
           (log/warn "[tmux-bridge] Preflight failed"
                     {:status (:adt/variant status)
                      :hint (remediation-hint status)}))
         status))))

(defn available?
  "Returns true if all preflight checks pass."
  ([] (available? {}))
  ([opts] (= :preflight/available (:adt/variant (run-preflight opts)))))

(defn ensure-python!
  "Ensure Python is initialized and libtmux is importable. Idempotent.

   Returns:
     true if Python is ready with libtmux available, false otherwise."
  ([] (ensure-python! {}))
  ([opts]
   (if @python-initialized?
     true
     (if (available? opts)
       (do
         (reset! python-initialized? true)
         (log/info "[tmux-bridge] Python initialized, libtmux available"
                   {:executable (or (:python-executable opts)
                                    default-python-executable
                                    "system default")})
         true)
       false))))

(defn reset-python!
  "Reset initialization state. For testing."
  []
  (reset! python-initialized? false)
  (reset! cached-status nil))

;;; =============================================================================
;;; Core Python Operations
;;; =============================================================================

(defn py-import
  "Safely import a Python module.
   Patches IO encoding before import (idempotent) for libtmux compat.

   Returns:
     Python module object or nil if unavailable."
  [module-name]
  (rescue nil
          (patch-io-encoding!)
          (let [import-fn (requiring-resolve 'libpython-clj2.python/import-module)]
            (import-fn module-name))))

(defn py-call
  "Call a Python method on an object.
   Supports kwargs via keyword args (e.g. :window_name \"foo\").
   Uses call-attr-kw to properly separate positional and keyword arguments.

   Returns method return value.
   Throws ExceptionInfo on failure."
  [obj method & args]
  (try
    (let [call-fn (requiring-resolve 'libpython-clj2.python/call-attr-kw)
          ;; Split args into positional and kwargs
          [positional kwargs] (loop [pos [] kw {} remaining args]
                                (if (empty? remaining)
                                  [pos kw]
                                  (let [a (first remaining)]
                                    (if (keyword? a)
                                      (recur pos
                                             (assoc kw a (second remaining))
                                             (drop 2 remaining))
                                      (recur (conj pos a) kw (rest remaining))))))]
      (call-fn obj method positional kwargs))
    (catch Exception e
      (throw (ex-info "Python call failed"
                      {:method method :error (ex-message e)}
                      e)))))

(defn py-attr
  "Get a Python object attribute.

   Returns attribute value or nil on failure."
  [obj attr]
  (rescue nil
          (let [attr-fn (requiring-resolve 'libpython-clj2.python/get-attr)]
            (attr-fn obj attr))))

(defn py->clj
  "Convert a Python collection to Clojure-iterable while preserving
   Python object types. Uses as-jvm (not ->jvm) to avoid deep conversion
   that strips Python type info from list elements (e.g. libtmux Pane objects)."
  [py-obj]
  (guard Exception py-obj
         (let [convert-fn (requiring-resolve 'libpython-clj2.python/as-jvm)]
           (convert-fn py-obj))))

(defn py-dict
  "Create a Python dict from a Clojure map."
  [m]
  (let [dict-fn (requiring-resolve 'libpython-clj2.python/->python)]
    (dict-fn m)))

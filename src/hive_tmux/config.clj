(ns hive-tmux.config
  "Typed config for hive-tmux's Python bridge.

   Currently exposes one knob:

     :python-executable — path to a Python interpreter for libpython-clj
                          to load. Repoints libpython-clj at a Python
                          where libtmux is installed (e.g. a conda env).

   Resolution chain (first non-nil wins, see `coalesce`):
     1. HIVE_PYTHON_EXECUTABLE env var
     2. HIVE_TMUX_PYTHON env var
     3. nil → caller falls back to host auto-detection or libpython-clj
              defaults."
  (:require [hive-di.core :refer [defconfig env coalesce]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defconfig PythonConfig
  :python-executable (coalesce [(env "HIVE_PYTHON_EXECUTABLE")
                                (env "HIVE_TMUX_PYTHON")]
                               :type     :string
                               :required false
                               :doc      "Python interpreter for libpython-clj. Should point at a Python with libtmux installed (typically a conda env)."))

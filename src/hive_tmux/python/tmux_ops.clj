(ns hive-tmux.python.tmux-ops
  "High-level libtmux operations — server/session/pane lifecycle.

   Topology: One tmux session named 'hive' (configurable).
   Each ling gets its own window (full-width, not a split).

   All operations are synchronous (<10ms) — no asyncio needed."
  (:require [hive-tmux.python.bridge :as py]
            [clojure.java.shell :as shell]
            [hive-dsl.result :refer [rescue guard]]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; =============================================================================
;;; Server Singleton
;;; =============================================================================

(defonce ^:private tmux-server (atom nil))

(def ^:private default-session-name "hive")

(defn ensure-server!
  "Connect to the running tmux server. Cached singleton.
   Returns the libtmux Server object or throws with actionable message."
  []
  (or @tmux-server
      (let [libtmux (py/py-import "libtmux")]
        (when-not libtmux
          (throw (ex-info (str "Cannot import libtmux Python package.\n"
                               "Fix: conda activate hive && pip install libtmux")
                          {:status :no-libtmux})))
        (let [server (guard Exception nil (py/py-call libtmux "Server"))]
          (when-not server
            (throw (ex-info
                    (str "Cannot connect to tmux server — is tmux running?\n"
                         "Fix: tmux new-session -d -s hive")
                    {:status :no-tmux-server})))
          (reset! tmux-server server)
          (log/info "[tmux-ops] Connected to tmux server")
          server))))

(defn reset-server!
  "Clear cached server. For testing."
  []
  (reset! tmux-server nil))

;;; =============================================================================
;;; Session Management
;;; =============================================================================

(defn- find-session
  "Find a tmux session by name. Returns session object or nil."
  [server session-name]
  (rescue nil
          (let [sessions (py/py-attr server "sessions")
                session-list (py/py->clj sessions)]
            (some (fn [s]
                    (when (= (str (py/py-attr s "name")) session-name)
                      s))
                  session-list))))

(defn ensure-session!
  "Get or create the hive tmux session.
   Falls back to `tmux new-session -d` shell command when libtmux's
   new_session fails (e.g. 'not a terminal' in headless/nREPL contexts).
   Returns the session object."
  ([] (ensure-session! {}))
  ([{:keys [session-name] :or {session-name default-session-name}}]
   (let [server (ensure-server!)]
     (or (find-session server session-name)
         ;; Try libtmux first, fall back to shell command
         (let [session (guard Exception nil
                              (py/py-call server "new_session"
                                          :session_name session-name
                                          :attach false))]
           (if session
             (do (log/info "[tmux-ops] Created tmux session via libtmux" {:name session-name})
                 session)
             ;; Fallback: shell command (works in non-TTY contexts)
             (do (let [{:keys [exit err]} (shell/sh "tmux" "new-session" "-d" "-s" session-name)]
                   (when-not (zero? exit)
                     (throw (ex-info (str "Failed to create tmux session via shell: " err)
                                     {:session-name session-name :exit exit}))))
                 (log/info "[tmux-ops] Created tmux session via shell" {:name session-name})
                 ;; Re-fetch the session object from the server
                 (find-session server session-name))))))))

;;; =============================================================================
;;; Pane Lifecycle
;;; =============================================================================

(defn create-pane!
  "Create a new window with a pane for a ling.
   Each ling gets its own window (full-width).

   Arguments:
     opts - {:ling-id str, :cwd str, :session-name str}

   Returns:
     {:pane-id str, :window-id str, :pane obj, :window obj}"
  [{:keys [ling-id cwd session-name]
    :or {session-name default-session-name}}]
  (let [session (ensure-session! {:session-name session-name})
        window-name (str "ling-" ling-id)
        window (if cwd
                 (py/py-call session "new_window"
                             :window_name window-name
                             :start_directory cwd
                             :attach false)
                 (py/py-call session "new_window"
                             :window_name window-name
                             :attach false))
        panes   (py/py-attr window "panes")
        pane    (py/py-call panes "__getitem__" 0)
        pane-id (str (py/py-attr pane "pane_id"))
        win-id  (str (py/py-attr window "window_id"))]
    (log/info "[tmux-ops] Created pane" {:ling-id ling-id
                                         :pane-id pane-id
                                         :window-id win-id
                                         :cwd cwd})
    {:pane-id   pane-id
     :window-id win-id
     :pane      pane
     :window    window}))

(defn send-keys!
  "Send keys to a tmux pane.

   Arguments:
     pane - libtmux pane object
     text - string to send
     opts - {:enter? bool} (default true — press Enter after text)"
  ([pane text] (send-keys! pane text {}))
  ([pane text {:keys [enter?] :or {enter? true}}]
   (py/py-call pane "send_keys" text :enter enter?)
   (log/debug "[tmux-ops] Sent keys to pane" {:pane-id (str (py/py-attr pane "pane_id"))
                                              :text-length (count text)})))

(defn capture-output
  "Capture the current pane content (last N lines).

   Arguments:
     pane - libtmux pane object
     opts - {:lines int} (default 50)

   Returns:
     Vector of strings (lines)."
  ([pane] (capture-output pane {}))
  ([pane {:keys [lines] :or {lines 50}}]
   (rescue []
           (let [raw (py/py-call pane "capture_pane" :start (- lines) :end -1)]
             (vec (py/py->clj raw))))))

(defn pane-alive?
  "Check if a pane still exists on the tmux server.

   Arguments:
     pane-id - string pane id (e.g. \"%42\")

   Returns:
     true if pane exists, false otherwise."
  [pane-id]
  (boolean
   (guard Exception false
          (let [server (ensure-server!)
                panes  (py/py-attr server "panes")
                ids    (set (map #(str (py/py-attr % "pane_id"))
                                 (py/py->clj panes)))]
            (contains? ids pane-id)))))

(defn kill-pane!
  "Kill a tmux pane (and its window if it's the only pane).

   Arguments:
     pane - libtmux pane object"
  [pane]
  (rescue nil
          (py/py-call pane "cmd" "kill-pane")
          (log/info "[tmux-ops] Killed pane" {:pane-id (str (py/py-attr pane "pane_id"))})))

(defn send-interrupt!
  "Send C-c interrupt to a tmux pane.

   Arguments:
     pane - libtmux pane object"
  [pane]
  (py/py-call pane "send_keys" "C-c" :enter false)
  (log/debug "[tmux-ops] Sent C-c to pane" {:pane-id (str (py/py-attr pane "pane_id"))}))

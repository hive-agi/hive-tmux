(ns hive-tmux.claude.sense
  "The sixth-sense classes the pane watcher's shouts become, so a supervisor
   hears pane changes with `swarm ss wait` instead of polling.

     :sense/pane-idle      a pane that worked settled idle
     :sense/goal-ended     a pane's /goal stopped being active
     :sense/context-high   a pane crossed the watch threshold upward
     :sense/pane-waiting   a pane waits on a human (permission or other prompt)
     :sense/tmux-job       a compact | say | goal job ended (detail job_status)

   sixth-sense belongs to hive-agent, which this addon does not depend on, so
   its registry is resolved softly (the hive-elm pattern): without it the
   shouts are still piggybacked, only never pushed.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def register-class-sym   'hive-agent.sixth-sense.classes/register-class!)
(def unregister-class-sym 'hive-agent.sixth-sense.classes/unregister-class!)
(def register-field-sym   'hive-agent.sixth-sense.detail/register-field!)
(def data-at-sym          'hive-agent.sixth-sense.detail/data-at)

(def event->class
  "Shout event type -> the sense class that perceives it."
  {:pane-idle    :sense/pane-idle
   :goal-ended   :sense/goal-ended
   :context-high :sense/context-high
   :pane-waiting :sense/pane-waiting
   :tmux-job     :sense/tmux-job})

(def ^:private docs
  {:pane-idle    "A Claude Code pane that was working has settled idle (idle on two watcher ticks)."
   :goal-ended   "A Claude Code pane's /goal is no longer active: it finished, or it was cleared."
   :context-high "A Claude Code pane's context crossed the watcher's threshold upward."
   :pane-waiting "A Claude Code pane is waiting on a human: a permission prompt or another question (session status :waiting)."
   :tmux-job     "A swarm tmux compact | say | goal job ended; detail job_status says how."})

(defn class-specs []
  (for [[event cls] event->class]
    {:sense/class cls
     :perceives?  (fn [shout] (= event (:event-type shout)))
     :doc         (docs event)}))

(def detail-keys
  "Shout data keys exposed on the sense, with their wire names."
  {:tmux-pane       "pane"
   :tmux-target     "pane_target"
   :tmux-name       "pane_name"
   :tmux-session-id "session_id"
   :tmux-cwd        "pane_cwd"
   :tmux-ctx-tokens "ctx_tokens"
   :tmux-goal-active "goal_active"
   :tmux-status     "pane_status"
   :tmux-job-id     "job_id"
   :tmux-job-kind   "job_kind"
   :tmux-job-status "job_status"
   :tmux-job-reason "job_reason"})

(defn soft-resolve [sym]
  (try (requiring-resolve sym) (catch Throwable _host-absent nil)))

(defn register!
  "Register the classes and their detail fields. {:classes n :fields n}; a
   no-op without sixth-sense."
  ([] (register! soft-resolve))
  ([resolve-fn]
   (let [register-class! (resolve-fn register-class-sym)
         register-field! (resolve-fn register-field-sym)
         data-at (resolve-fn data-at-sym)]
     {:classes (if register-class! (count (doall (map register-class! (class-specs)))) 0)
      :fields (if (and register-field! data-at)
                (count (doall (for [[k wire] detail-keys]
                                (register-field! {:detail/key k :wire wire :read (data-at k)}))))
                0)})))

(defn unregister!
  ([] (unregister! soft-resolve))
  ([resolve-fn]
   (when-let [unregister-class! (resolve-fn unregister-class-sym)]
     (doseq [cls (vals event->class)] (unregister-class! cls)))
   nil))

(ns hive-tmux.vessel-target
  "The hive-vessel target of this addon: :text natives executed through the
   tmux CLI by hive-vessel.executor.tmux, on the session the python bridge
   drives.

   Strata: `target-opts` is the pure Promote step (addon config -> executor
   opts); `target` is the Boundary that hands those opts to hive-vessel's
   executor; `hooks-for` is the addon-facing projection, resolved per call so
   a consumer holding the hook sees nil once the addon is shut down."
  (:require [hive-vessel.executor.tmux :as tmux-exec]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def target-hook-key
  "The hook key under which an active addon offers its target: a 0-arity fn
   returning the descriptor, or nil once shut down. The same key hive-emacs
   and hive-vscode use, so a consumer asks every editor addon one question."
  :vessel/target)

(def default-session
  "The tmux session panels open in when the config names none: the one the
   python bridge creates (`tmux new-session -d -s hive`)."
  "hive")

(def Config
  "The slice of the addon's runtime config the target reads. Every key is
   optional and nil means unset, so a host that passes its whole config map
   through is accepted."
  [:map
   [:tmux/session {:optional true} [:maybe [:string {:min 1}]]]
   [:tmux/socket-name {:optional true} [:maybe [:string {:min 1}]]]
   [:tmux/binary {:optional true} [:maybe [:string {:min 1}]]]
   [:tmux/editor {:optional true} [:maybe [:string {:min 1}]]]
   [:vessel/features {:optional true} [:maybe [:set :keyword]]]])

(def TargetOpts
  "What hive-vessel.executor.tmux/target accepts from this addon. Closed: a
   config key that is not one of these never reaches the executor."
  [:map {:closed true}
   [:session [:string {:min 1}]]
   [:socket-name {:optional true} [:string {:min 1}]]
   [:tmux {:optional true} [:string {:min 1}]]
   [:editor {:optional true} [:string {:min 1}]]
   [:features {:optional true} [:set :keyword]]])

(defn target-opts
  "Executor opts from the addon CONFIG. The session defaults to
   `default-session`; every other key is passed through only when set."
  [config]
  (into {:session (or (:tmux/session config) default-session)}
        (remove (comp nil? val))
        {:socket-name (:tmux/socket-name config)
         :tmux (:tmux/binary config)
         :editor (:tmux/editor config)
         :features (:vessel/features config)}))

(m/=> target-opts [:=> [:cat Config] TargetOpts])

(defn target
  "A hive-vessel target for this tmux: {:vessel/id :tmux :vessel/dialect :text
   :vessel/execute! f}. CONFIG is the addon config (see `Config`); PORTS may
   carry :run!, the tmux port hive-vessel's executor calls instead of the
   binary, which is how a test observes the executor without a tmux server."
  ([] (target {}))
  ([config] (target config {}))
  ([config ports]
   (tmux-exec/target (merge (target-opts config) (select-keys ports [:run!])))))

(defn hooks-for
  "The hook contributions of an addon whose lifecycle lives in the STATE atom
   ({:initialized? bool :config map}): the target under `target-hook-key`
   while initialized, {} otherwise. The hook fn re-reads STATE on every call,
   so a consumer that kept the fn gets nil after shutdown instead of a target
   for a server nobody drives."
  [state]
  (if (:initialized? @state)
    {target-hook-key (fn [] (when (:initialized? @state)
                              (target (:config @state))))}
    {}))

(m/=> hooks-for [:=> [:cat [:fn #(instance? clojure.lang.IDeref %)]]
                 [:map-of :keyword fn?]])

(ns hive-tmux.claude.interrupt
  "When may a compact job cut into a working turn? Pure: a screen and a row
   in, a verdict out. `compact` performs what this decides.

   A /goal coordinator can keep ONE turn alive for hours by looping on
   100 s `ss wait` calls (friction 20261121180326-2ea1d54b), so a job that
   waits for a turn boundary never sees one. Interrupting is the way out,
   but a keystroke into a working turn can cut a commit, a `ship land` or a
   `kubectl apply` in half. So the rule is narrow:

     * never while text stands at the prompt (someone's draft), nor while a
       dialog is open (on screen, or the session :waiting on a human);
     * never while the running tool reads as a WRITE (`write-markers`);
     * under :auto, only on a pane whose /goal was active when the job began,
       after it waited :interrupt-after-ms for rest, and only while the
       running tool reads as a read-only wait (`read-only-markers`) or no
       tool runs at all (the model is thinking, the prompt is empty);
     * under true, the goal and wait gates are dropped and an unclassified
       tool is accepted; the write and prompt gates still hold.

   How the interrupt is made is the caller's choice (:interrupt-via):
     :force-send  type /compact (Claude Code queues it: `Queued · ctrl+x
                  ctrl+s to send now`), then C-x C-s. Measured on Claude Code
                  2.1.330: a running foreground Bash is moved to the
                  background, not killed, and the compaction starts at once.
     :escape      Esc (up to 3 tries) until the spinner is gone, then /compact
                  as usual (the raw-tmux recipe recycle-goal-pane.sh). Esc
                  kills the running tool call.

   Stratum: Promote. No tmux, no clock."
  (:require [clojure.string :as str]
            [hive-tmux.claude.screen :as screen]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def write-markers
  "Substrings (lower case) of a tool call that may be writing. Checked FIRST,
   so a call that also looks like a wait (`ship land ... wait_s`) is a write."
  ["git" "ship" "land" "commit" "push" "merge" "rebase" "kubectl" "helm" "apply"
   "deploy" "migrat" "terraform" "docker" "psql" "write(" "edit(" "multiedit("
   "write-form" "raw-splice" "find-replace" "refactor-tx" "rm " "mv " "cp "
   "tee " "> " "install" "publish" "release" "delete" "create" "kill"])

(def read-only-markers
  "Substrings (lower case) of a tool call that only waits or reads."
  ["ss wait" "await" "sleep" "wait_ms" "timeout_ms" "collect" "watch"
   "monitor" "status" "tail " "read("])

(defn- lc [s] (str/lower-case (str s)))

(defn- marker-re
  "A marker found only where a word starts: `rm ` must not match the `swarm `
   of `hive - swarm (MCP)`, nor `git` the end of `digit`."
  [m]
  (re-pattern (str "(?<![a-z0-9_])" (java.util.regex.Pattern/quote m))))

(def ^:private write-res (mapv marker-re write-markers))
(def ^:private read-only-res (mapv marker-re read-only-markers))

(defn tool-kind
  "How TEXT, a tool call as the screen shows it, reads: :write, :read-only or
   :unknown. A write wins any tie: misreading a write as a wait is the one
   mistake that matters."
  [text]
  (let [t (lc text)]
    (cond (some #(re-find % t) write-res) :write
          (some #(re-find % t) read-only-res) :read-only
          :else :unknown)))

(defn- lead [line] (count (take-while #(or (= \space %) (= \u00a0 %)) (str line))))

(defn- running-line? [line]
  (let [t (screen/utrim line)]
    (and (str/starts-with? t "⎿") (str/includes? t "Running…"))))

(defn- call-lines
  "The lines of the tool call whose `⎿ Running…` line is at R: walk up while
   a line is the call's own (indented at most 2, not output, not blank),
   ending on the line that opens with `●`. Its bullet may be blinked off."
  [above r]
  (loop [i (dec r) acc ()]
    (if (neg? i)
      acc
      (let [l (nth above i) t (str/triml l)]
        (cond (str/starts-with? t "●") (cons l acc)
              (or (str/blank? l) (str/starts-with? t "⎿") (> (lead l) 2)) acc
              :else (recur (dec i) (cons l acc)))))))

(defn running-tool
  "The tool call still running on LINES (a pane capture, escapes kept or not),
   as {:tool text :kind kw}; nil when none runs. It is the call above the LAST
   `⎿ Running…` line over the prompt. A newer `●` line under it is a call
   the screen has not finished drawing: :unknown."
  [lines]
  (let [lines (mapv screen/strip-ansi lines)
        n (count lines)
        prompt (or (some #(when (str/starts-with? (str/triml (nth lines %)) "❯") %)
                         (range (dec n) -1 -1))
                   n)
        above (subvec lines 0 prompt)
        r (some #(when (running-line? (nth above %)) %) (range (dec (count above)) -1 -1))]
    (when r
      (let [text (not-empty (str/join " " (map screen/utrim (call-lines above r))))
            text (some-> text (str/replace #"^●\s*" ""))
            newer? (some #(str/starts-with? (str/triml %) "●") (subvec above (inc r)))]
        (cond newer? {:tool text :kind :unknown :newer-call? true}
              text {:tool text :kind (tool-kind text)}
              :else {:tool nil :kind :unknown})))))

(defn mode-param
  "The :interrupt option from a parameter: true, false or :auto (anything
   unrecognised is :auto, whose gates are the strictest that still act)."
  [x]
  (cond (true? x) true
        (false? x) false
        (nil? x) :auto
        :else (let [s (lc (str/trim (if (keyword? x) (name x) (str x))))]
                (cond (#{"true" "1" "yes" "on"} s) true
                      (#{"false" "0" "no" "off"} s) false
                      :else :auto))))

(defn via-param
  "The :interrupt-via option: :escape, else :force-send."
  [x]
  (if (= "escape" (lc (if (keyword? x) (name x) x))) :escape :force-send))

(defn verdict
  "May the job interrupt now? OPTS: :interrupt (true | false | :auto) and
   :interrupt-after-ms. FACTS: :row (the pane now), :tool (`running-tool`),
   :waited-ms (since the job began to wait for rest), :goal-at-start? (its
   /goal was active when the job began). {:interrupt? true :tool ...}, or
   {:hold reason ...}: a hold is not final, the job reads again next poll."
  [{:keys [interrupt interrupt-after-ms]} {:keys [row tool waited-ms goal-at-start?]}]
  (let [mode (if (nil? interrupt) :auto interrupt)
        kind (:kind tool)]
    (cond
      (false? mode) {:hold :interrupt-off}
      (nil? row) {:hold :unreadable}
      (not (:busy? row)) {:hold :not-busy}
      (:input row) {:hold :prompt-holds-text :input (:input row)}
      (or (:dialog row) (= :waiting (:status row))) {:hold :open-dialog}
      (= :write kind) {:hold :write-in-flight :tool (:tool tool)}
      (true? mode) {:interrupt? true :tool tool}
      (not goal-at-start?) {:hold :no-goal}
      (< (or waited-ms 0) (or interrupt-after-ms 0)) {:hold :waiting :waited-ms waited-ms}
      (= :unknown kind) {:hold :unknown-tool :tool (:tool tool)}
      :else {:interrupt? true :tool tool})))

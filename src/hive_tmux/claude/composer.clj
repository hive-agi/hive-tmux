(ns hive-tmux.claude.composer
  "The prompt line (composer) of a Claude Code pane: type text into it and,
   for a slash command, read it back before submitting it.

   Stratum: Pipeline, under `hive-tmux.claude.compact`. Every effect (tmux,
   sleep) goes through the host port."
  (:require [clojure.string :as str]
            [hive-tmux.claude.panes :as panes]
            [hive-tmux.claude.screen :as screen]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- squash
  "S without any whitespace, no-break spaces included."
  [s]
  (apply str (remove #(or (Character/isWhitespace ^char %) (Character/isSpaceChar ^char %)) (str s))))

(defn holds?
  "Does INPUT, the text a pane's composer shows, hold TEXT as it was typed?
   Whitespace is ignored, since a long line wraps inside the prompt box, and a
   tail after TEXT is allowed, since Claude Code draws a dim argument hint
   after a slash command. A paste collapsed to `[Pasted text #1 +4 lines]`,
   or a command that lost its `/`, does not hold it."
  [text input]
  (and (some? input) (str/starts-with? (squash input) (squash text))))

(defn input-now
  "What the composer of PANE-ID shows now, read from its screen. Nil when it
   is empty or the screen cannot be read."
  [h pane-id]
  (:input (screen/read-screen (or (panes/capture h pane-id 40) []))))

(def ^:private read-back-tries 5)

(defn- read-back
  "Nil once the composer of PANE-ID holds TEXT, read a few times over about a
   second while the pane draws it; else {:composer <what it showed last>}."
  [{:keys [sleep!] :as h} pane-id text]
  (loop [n read-back-tries]
    (let [seen (input-now h pane-id)]
      (cond (holds? text seen) nil
            (zero? n) {:composer seen}
            :else (do (sleep! 200) (recur (dec n)))))))

(defn send-text!
  "Type TEXT into PANE-ID and submit it. One line goes as literal keys; text
   with newlines is pasted as one bracketed frame so no LF submits early. A
   pane in vim NORMAL mode is put in insert mode first.

   A slash command is read back before Enter: Claude Code runs it only when
   the composer still starts with it, and a paste collapsed to `[Pasted text]`
   or a dropped `/` would go out as a plain prompt instead. Then Enter is not
   pressed and the text stays in the composer. Returns {:submitted? true}, or
   {:submitted? false :composer <what the composer showed>}."
  [{:keys [tmux! sleep!] :as h} pane-id text vim-mode]
  (when (= :normal vim-mode)
    (tmux! ["send-keys" "-t" pane-id "i"]))
  (if (str/includes? text "\n")
    (let [buf (str "hive-claude-" (random-uuid))]
      (tmux! ["load-buffer" "-b" buf "-"] text)
      (tmux! ["paste-buffer" "-p" "-r" "-d" "-b" buf "-t" pane-id]))
    (tmux! ["send-keys" "-t" pane-id "-l" text]))
  (sleep! 300)
  (if-let [mismatch (when (str/starts-with? text "/") (read-back h pane-id text))]
    (assoc mismatch :submitted? false)
    (do (tmux! ["send-keys" "-t" pane-id "Enter"])
        {:submitted? true})))

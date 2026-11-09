(ns hive-tmux.claude.screen
  "Pure reading of a Claude Code pane: the captured screen lines of one tmux
   pane in, the facts a supervisor needs out.

   Stratum: Promote. Nothing here touches tmux, the filesystem or a clock; the
   lines come from `hive-tmux.claude.panes`, which is the Boundary.

   What the screen carries (Claude Code 2.1.x):

     * Working… (26m 48s · ↓ 29.8k tokens)      <- spinner line while a turn runs
         260803 tokens · ◎ /goal active (26m)   <- status line: context size, goal
     ──────────── caveman skill hydration ─       <- footer rule naming the session
     ❯                                            <- the prompt
       -- INSERT -- ⏵⏵ auto mode on               <- vim mode + permission mode

   The context count is the bare `NNN tokens` that opens the status line. The
   spinner line also says `29.8k tokens`, which is the turn's output, so only a
   first segment made of digits and ` tokens` counts.

   Matching is plain string work, no regex: every shape here is a fixed glyph
   or word at a fixed edge of a line."
  (:require [clojure.string :as str]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def Screen
  "What one pane's screen says. A fact is nil when the screen does not say it,
   which is different from false: an unreadable pane is not an idle one."
  [:map
   [:claude? :boolean]
   [:ctx-tokens [:maybe nat-int?]]
   [:busy? [:maybe :boolean]]
   [:goal-active? :boolean]
   [:goal-age [:maybe :string]]
   [:footer-name [:maybe :string]]
   [:vim-mode [:maybe [:enum :insert :normal]]]
   [:activity [:maybe :string]]
   [:input [:maybe :string]]])

(def ^:private segment-sep " · ")
(def ^:private goal-mark "◎ /goal active")
(def ^:private ellipsis "…")
(def ^:private rule-char \─)

(defn- cut
  "[before after] of S at the first SEP, or [s nil] when SEP is absent."
  [s sep]
  (if-let [i (str/index-of s sep)]
    [(subs s 0 i) (subs s (+ i (count sep)))]
    [s nil]))

(defn- digits? [s]
  (and (seq s) (every? #(Character/isDigit ^char %) s)))

(defn- word-char? [c]
  (Character/isLetterOrDigit ^char c))

(defn ctx-tokens-of
  "The context size a status LINE opens with (`260803 tokens · …`), or nil."
  [line]
  (let [seg (str/trim (first (cut (str line) segment-sep)))
        n (when (str/ends-with? seg " tokens")
            (subs seg 0 (- (count seg) (count " tokens"))))]
    (when (digits? n) (parse-long n))))

(defn goal-of
  "{:age str-or-nil} when LINE shows an active /goal, else nil."
  [line]
  (let [[_ after] (cut (str line) goal-mark)]
    (when after
      (let [[gap inner] (cut after "(")
            [age closed] (when inner (cut inner ")"))]
        {:age (when (and (str/blank? gap) closed) age)}))))

(defn activity-of
  "The running-turn text of a spinner LINE (`✶ Percolating… (23m 49s · …)`), or
   nil. The line opens with one non-word glyph, then a word ending in `…`."
  [line]
  (let [t (str/trim (str line))
        [glyph more] (cut t " ")
        [word tail] (when more (cut more " "))]
    (when (and word (= 1 (count glyph)) (not (word-char? (first glyph)))
               (str/ends-with? word ellipsis) (> (count word) 1))
      (let [tail (str/trim (or tail ""))
            inner (when (str/starts-with? tail "(")
                    (first (cut (subs tail 1) ")")))]
        (str word (when inner (str " (" (str/trim inner) ")")))))))

(defn footer-name-of
  "The session name on a footer rule LINE (`──── name ─`), or nil."
  [line]
  (let [t (str/trim (str line))]
    (when (and (> (count t) 2)
               (= rule-char (first t)) (= rule-char (last t)))
      (not-empty (str/trim (str/replace t (str rule-char) " "))))))

(defn- prompt? [line]
  (str/starts-with? (str/triml (str line)) "❯"))

(defn- prompt-index
  "Index of the last prompt line, or nil. Facts are read only above it, so a
   prompt the user is typing cannot fake them."
  [lines]
  (some (fn [i] (when (prompt? (nth lines i)) i))
        (range (dec (count lines)) -1 -1)))

(defn- last-some [f lines]
  (some f (rseq (vec lines))))

(defn- vim-mode [lines]
  (cond (some #(str/includes? % "-- INSERT --") lines) :insert
        (some #(str/includes? % "-- NORMAL --") lines) :normal
        :else nil))

(def ^:private unread
  {:claude? false :ctx-tokens nil :busy? nil :goal-active? false :goal-age nil :footer-name nil :vim-mode nil :activity nil :input nil})

(defn- space? [c]
  (or (Character/isWhitespace ^char c) (Character/isSpaceChar ^char c)))

(defn utrim
  "S without leading and trailing space of any kind. `str/trim` keeps U+00A0,
   and Claude Code draws its empty prompt as `❯` + NO-BREAK SPACE, so a plain
   trim reads every idle prompt as pending input."
  [s]
  (let [s (str s)
        b (count (take-while space? s))
        e (- (count s) (count (take-while space? (reverse s))))]
    (if (< b e) (subs s b e) "")))

(defn read-screen
  "The facts of LINES, a pane capture top to bottom. A screen with no prompt
   is `:claude? false` with every fact nil: not a Claude Code pane, or one
   mid-redraw, and either way nothing on it can be trusted.

   :input is whatever stands after `❯` on the prompt line. Text there was
   typed by someone (or is a dimmed suggestion, which a plain capture cannot
   tell apart); a sender must not type over it."
  [lines]
  (let [lines (vec lines)]
    (if-let [p (prompt-index lines)]
      (let [above (subvec lines (max 0 (- p 12)) p)
            goal (last-some goal-of above)
            activity (last-some activity-of above)
            prompt-line (str/triml (nth lines p))]
        {:claude? true
         :ctx-tokens (last-some ctx-tokens-of above)
         :busy? (boolean activity)
         :goal-active? (boolean goal)
         :goal-age (:age goal)
         :footer-name (last-some footer-name-of above)
         :vim-mode (vim-mode (subvec lines p))
         :activity activity
         :input (not-empty (utrim (subs prompt-line 1)))})
      unread)))

(m/=> read-screen [:=> [:cat [:sequential :string]] Screen])

(defn title-name
  "The session name in a tmux pane title such as \"✳ caveman skill load\": the
   title minus its leading status glyphs. Nil for a blank title."
  [title]
  (let [t (str/trim (str title))]
    (not-empty (str/trim (apply str (drop-while (complement word-char?) t))))))

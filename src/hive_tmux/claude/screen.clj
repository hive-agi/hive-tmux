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
   which is different from false: an unreadable pane is not an idle one.

   :input is text someone typed at the prompt; :suggestion is the dimmed
   (SGR 2) ghost text Claude Code predicts there, which no one typed."
  [:map
   [:claude? :boolean]
   [:ctx-tokens [:maybe nat-int?]]
   [:busy? [:maybe :boolean]]
   [:goal-active? :boolean]
   [:goal-ended? {:optional true} :boolean]
   [:goal-age [:maybe :string]]
   [:footer-name [:maybe :string]]
   [:vim-mode [:maybe [:enum :insert :normal]]]
   [:activity [:maybe :string]]
   [:input [:maybe :string]]
   [:suggestion [:maybe :string]]])

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

(def ^:private goal-achieved-mark "Goal achieved")

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
  {:claude? false :ctx-tokens nil :busy? nil :goal-active? false :goal-age nil :footer-name nil :vim-mode nil :activity nil :input nil :suggestion nil})

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

;;; Escapes. Lines come from `tmux capture-pane -e`, which keeps SGR runs:
;;; the only way to tell Claude Code's dimmed prompt suggestion from text a
;;; human typed is that the suggestion is drawn in SGR 2 (faint).

(def ^:private esc \u001b)

(defn- split-semi
  "S split at every `;`, keeping empty fields."
  [s]
  (loop [acc [] from 0]
    (if-let [i (str/index-of s ";" from)]
      (recur (conj acc (subs s from i)) (inc i))
      (conj acc (subs s from)))))

(defn- sgr-dim
  "DIM after applying the SGR parameter string PARAMS (`38;5;246;2`): 2 sets
   faint, 0 (or an empty list) and 22 clear it; the operands of an extended
   colour (38/48/58 ;5;n and ;2;r;g;b) are skipped, so the 2 of `38;5;2` is a
   colour, not faint."
  [dim params]
  (loop [ps (if (str/blank? params) ["0"] (split-semi params)) dim dim]
    (if-let [[p & more] (seq ps)]
      (case p
        ("" "0" "22") (recur more false)
        "2" (recur more true)
        ("38" "48" "58") (case (first more)
                           "5" (recur (drop 2 more) dim)
                           "2" (recur (drop 4 more) dim)
                           (recur more dim))
        (recur more dim))
      dim)))

(defn- skip-escape
  "Index just past the escape sequence starting at I in S, and its SGR params
   when it is an SGR (`ESC [ ... m`), as [end params-or-nil]. CSI, OSC (ended
   by BEL or ESC \\) and two-byte escapes are all consumed."
  [^String s i]
  (let [n (count s)
        nxt (when (< (inc i) n) (.charAt s (inc i)))]
    (case nxt
      \[ (let [end (loop [j (+ i 2)]
                     (cond (>= j n) n
                           (<= 0x40 (int (.charAt s j)) 0x7e) j
                           :else (recur (inc j))))]
           (if (< end n)
             [(inc end) (when (= \m (.charAt s end)) (subs s (+ i 2) end))]
             [n nil]))
      \] (loop [j (+ i 2)]
           (cond (>= j n) [n nil]
                 (= \u0007 (.charAt s j)) [(inc j) nil]
                 (and (= esc (.charAt s j)) (< (inc j) n) (= \\ (.charAt s (inc j)))) [(+ j 2) nil]
                 :else (recur (inc j))))
      nil [n nil]
      [(+ i 2) nil])))

(defn sgr-runs
  "LINE as [[text dim?] ...]: the visible text in order, split where SGR
   faint turns on or off, escapes removed. Adjacent runs never share dim?."
  [line]
  (let [s (str line) n (count s)]
    (loop [i 0 dim false buf (StringBuilder.) runs []]
      (let [flush (fn [runs] (if (pos? (.length buf))
                               (let [t (str buf)]
                                 (.setLength buf 0)
                                 (if (= dim (second (peek runs)))
                                   (conj (pop runs) [(str (first (peek runs)) t) dim])
                                   (conj runs [t dim])))
                               runs))]
        (cond
          (>= i n) (flush runs)
          (= esc (.charAt s i))
          (let [[end params] (skip-escape s i)
                dim' (if params (sgr-dim dim params) dim)]
            (if (= dim dim')
              (recur end dim buf runs)
              (let [runs (flush runs)] (recur end dim' buf runs))))
          :else (do (.append buf (.charAt s i)) (recur (inc i) dim buf runs)))))))

(defn strip-ansi
  "LINE without escape sequences."
  [line]
  (let [s (str line)]
    (if (str/includes? s (str esc))
      (apply str (map first (sgr-runs s)))
      s)))

(defn- prompt-split
  "{:input typed :suggestion dim} of a prompt LINE (escapes kept), each the
   text after `❯`, space-trimmed, nil when empty. Dim (SGR 2) text is Claude
   Code's ghost suggestion; everything else was typed."
  [line]
  (let [after (loop [[[t d :as run] & more] (sgr-runs line)]
                (cond (nil? run) nil
                      (str/includes? t "❯") (let [i (str/index-of t "❯")]
                                              (cons [(subs t (inc i)) d] more))
                      :else (recur more)))
        text (fn [want] (not-empty (utrim (apply str (keep (fn [[t d]] (when (= want d) t)) after)))))]
    {:input (text false) :suggestion (text true)}))

(defn read-screen
  "The facts of LINES, a pane capture top to bottom, escapes kept (`tmux
   capture-pane -e`) or not. A screen with no prompt is `:claude? false` with
   every fact nil: not a Claude Code pane, or one mid-redraw, and either way
   nothing on it can be trusted.

   :input is what stands after `❯` on the prompt line and was typed by
   someone; a sender must not type over it. :suggestion is the dimmed (SGR 2)
   ghost text Claude Code predicts there: no one typed it, a sender may type
   over it. A capture without escapes cannot tell them apart and reads all of
   it as :input, the safe side.

   :goal-ended? is true when no goal is active and the screen still shows
   Claude Code's `Goal achieved` line: the pane finished its goal and waits
   for new work."
  [lines]
  (let [raw (vec lines)
        lines (mapv strip-ansi raw)]
    (if-let [p (prompt-index lines)]
      (let [above (subvec lines (max 0 (- p 12)) p)
            goal (last-some goal-of above)
            activity (last-some activity-of above)
            {:keys [input suggestion]} (prompt-split (nth raw p))]
        {:claude? true
         :ctx-tokens (last-some ctx-tokens-of above)
         :busy? (boolean activity)
         :goal-active? (boolean goal)
         :goal-ended? (boolean (and (not goal) (some #(str/includes? % goal-achieved-mark) (subvec lines 0 p))))
         :goal-age (:age goal)
         :footer-name (last-some footer-name-of above)
         :vim-mode (vim-mode (subvec lines p))
         :activity activity
         :input input
         :suggestion suggestion})
      unread)))

(m/=> read-screen [:=> [:cat [:sequential :string]] Screen])

(defn title-name
  "The session name in a tmux pane title such as \"✳ caveman skill load\": the
   title minus its leading status glyphs. Nil for a blank title."
  [title]
  (let [t (str/trim (str title))]
    (not-empty (str/trim (apply str (drop-while (complement word-char?) t))))))

(defn- rule-line? [line]
  (let [t (utrim line)]
    (and (seq t) (every? #(= rule-char %) t))))

(defn- finished-turn?
  "`✻ Worked for 3m 12s` (any verb): one glyph, a word, `for`, a duration."
  [t]
  (let [[glyph more] (cut t " ")
        [_word tail] (when more (cut more " "))]
    (boolean (and tail (= 1 (count glyph)) (not (word-char? (first glyph)))
                  (str/starts-with? tail "for ")
                  (some-> (subs tail 4) first Character/isDigit)))))

(defn- chrome? [line]
  (let [t (utrim line)]
    (or (str/blank? t)
        (rule-line? line)
        (some? (ctx-tokens-of line))
        (some? (goal-of line))
        (some? (activity-of line))
        (some? (footer-name-of line))
        (finished-turn? t)
        (and (str/starts-with? t "⎿") (str/starts-with? (utrim (subs t 1)) "Tip:")))))

(defn body
  "The conversation part of LINES: everything above the prompt minus the
   chrome Claude Code draws under it (spinner, finished-turn line, status line,
   tip, footer rule), trailing blank lines dropped, at most N lines from the
   bottom. A screen with no prompt is returned whole, last N lines. No-break
   spaces, which Claude Code draws inside its chrome (`⎿` + U+00A0 + `Tip:`),
   read as plain spaces, and escapes (a `capture-pane -e` capture) are removed."
  [lines n]
  (let [lines (mapv #(str/replace (strip-ansi %) "\u00a0" " ") lines)
        above (if-let [p (prompt-index lines)] (subvec lines 0 p) lines)
        kept (->> (rseq above) (drop-while chrome?) reverse vec)]
    (vec (take-last n kept))))

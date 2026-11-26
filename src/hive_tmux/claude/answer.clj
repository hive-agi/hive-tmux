(ns hive-tmux.claude.answer
  "Answer the option menu a Claude Code pane is showing.

   Parsing and selection are pure. All screen reads and key writes go through
   the existing host port; this namespace never starts a compaction job.

   This verb sits on the operator's consent boundary. It answers only a menu
   it can positively read, refuses a permission dialog unless the caller
   passes permission? with an expect text from the dialog, never chooses a
   standing grant, re-reads the pane before Enter, and audits every attempt
   to a file whether or not a job is running on the pane."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-tmux.claude.compact :as compact]
            [hive-tmux.claude.panes :as panes]
            [hive-tmux.claude.screen :as screen]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def defaults
  {:capture-lines 120
   :confirm-tries 5
   :confirm-delay-ms 200})

(defn- visible-lines
  "Screen lines without escapes or trailing space, no-break spaces read as
   spaces. Leading space is kept: option and description columns need it."
  [text]
  (->> (if (sequential? text) text (str/split-lines (str text)))
       (mapv #(str/trimr (str/replace (screen/strip-ansi %) "\u00a0" " ")))))

(def ^:private option-pattern
  "An option row: indent, an optional single `❯` highlight, a 1-2 digit
   number, a dot, the label. `>` is not a highlight."
  #"^( *)(❯ +)?([0-9]{1,2})\. +(\S.*)$")

(defn- option-line
  "LINE as an option row {:col :number :label :highlighted?}, or nil. :col is
   the column of the number, which every row of one menu shares."
  [line]
  (when-let [[_ indent marker digits label] (re-matches option-pattern line)]
    (let [number (Long/parseLong digits)]
      (when (pos? number)
        {:col (+ (count indent) (count marker))
         :number number
         :label (str/trim label)
         :highlighted? (some? marker)}))))

(defn- indent [line]
  (count (take-while #(= \space %) line)))

(defn- rule? [line]
  (let [t (str/trim line)]
    (and (< 2 (count t)) (every? #{\─ \╌ \━ \═} t))))

(defn- scroll-marker? [line]
  (boolean (re-matches #"\s*[↑↓] .*\b(?:more|earlier|later)\b.*" line)))

(defn- footer?
  "Claude Code's selection footer, alone on its line."
  [line]
  (let [t (str/trim line)]
    (or (str/starts-with? t "Enter to select") (str/starts-with? t "Esc to cancel"))))

(defn- composer?
  "A line that shows the prompt composer or a running turn: a `❯` that is not
   an option row, a vim mode line or a spinner."
  [line]
  (let [t (str/trim line)]
    (or (and (str/starts-with? t "❯") (nil? (option-line line)))
        (str/includes? t "-- INSERT --")
        (str/includes? t "-- NORMAL --")
        (some? (screen/activity-of t)))))

(def ^:private checkbox-pattern #"^(?:\[[ xX✔✓]\]|[☐☑☒◯◉]).*")

(def ^:private persistent-grant-pattern
  "A backstop for every menu: wording of a standing grant."
  #"(?i)don['’]?t ask again|allow all|during this session|for this session|always allow|auto-accept|bypass permissions|trust this folder|working director")

(defn- one-shot-label?
  "Is LABEL a one-shot answer to a permission dialog? An allow-list: exactly
   `yes`, exactly `no`, or `no, ...`. Anything else (auto-accept, trust this
   folder, add a directory, don't ask again, ...) may be a standing grant."
  [label]
  (let [l (-> (str label) str/lower-case str/trim (str/replace #"[.!]+$" ""))]
    (or (= "yes" l) (= "no" l) (str/starts-with? l "no,"))))

(defn- normalized [s]
  (-> (str s) screen/utrim str/lower-case (str/replace #"\s+" " ")))

(defn- block-around
  "The option rows of the one contiguous menu block around index H, a `❯`
   option row: rows at H's number column, joined only by description lines
   indented deeper than it, by a scroll marker or by blank lines. Returns
   {:rows [{:index :number :label :highlighted?}] :start :end}."
  [lines h]
  (let [col (:col (option-line (nth lines h)))
        row-at (fn [i] (let [o (option-line (nth lines i))]
                         (when (and o (= col (:col o))) (assoc o :index i))))
        joiner? (fn [i] (let [l (nth lines i)]
                          (or (str/blank? l) (scroll-marker? l) (rule? l)
                              (> (indent l) col))))
        walk (fn [step]
               (loop [i (step h) acc []]
                 (if (and (< -1 i (count lines)))
                   (if-let [o (row-at i)]
                     (recur (step i) (conj acc o))
                     (if (joiner? i) (recur (step i) acc) acc))
                   acc)))
        up (reverse (walk dec))
        down (walk inc)
        rows (vec (concat up [(row-at h)] down))]
    {:rows rows :start (:index (first rows)) :end (:index (peek rows))}))

(defn- question-of
  "The non-blank lines above START, up to 20, stopping at the solid rule that
   opens the dialog. Dashed rules (`╌`) inside a dialog frame the command and
   are kept, so expect= can bind to the command text."
  [lines start]
  (->> (subvec lines 0 start)
       rseq
       (take-while #(not (and (rule? %) (not (str/includes? % "╌")))))
       (remove str/blank?)
       (take 30)
       reverse
       (mapv str/trim)))

(defn- kind-of
  "Fail closed: only the AskUserQuestion footer (`Enter to select` with the
   `↑/↓ to navigate` hint) is positive evidence of a question menu. Every
   other dialog, whatever its wording, is a permission dialog."
  [footer]
  (if (and (str/includes? footer "Enter to select") (str/includes? footer "to navigate"))
    :ask-user-question
    :permission-dialog))

(defn parse-menu
  "Read the live Claude Code option menu from captured screen TEXT (a string or
   screen lines), escapes kept or not.

   The menu is the one contiguous block around the single `❯` option row:
   option rows that share its number column, joined by deeper-indented
   description lines, scroll markers or blank lines. It needs strictly
   consecutive 1-2 digit numbers, at least two rows, and a selection footer
   (`Enter to select` / `Esc to cancel`) below it with nothing between but
   blanks, rules, scroll markers or a trailing free-text row. A visible
   composer or running turn below the menu, a checkbox (multi-select) menu,
   or anything else is refused.

   Returns {:menu {...}} with :kind, :question, :options, :highlighted,
   :footer and :fingerprint, or {:error reason} with reason one of :no-menu,
   :no-highlight, :not-a-menu, :composer-visible or :multi-select."
  [text]
  (let [lines (visible-lines text)
        marked (filterv #(:highlighted? (option-line (nth lines %))) (range (count lines)))
        footer-i (some #(when (footer? (nth lines %)) %) (range (dec (count lines)) -1 -1))]
    (cond
      (nil? footer-i)
      {:error (if (seq marked) :not-a-menu :no-menu)}

      (some #(or (composer? %) (str/starts-with? (str/trim %) "❯")) (subvec lines footer-i))
      {:error :composer-visible}

      :else
      (let [floor (or (some #(when (footer? (nth lines %)) %) (range (dec footer-i) -1 -1)) -1)
            live (filterv #(< floor % footer-i) marked)]
        (if (not= 1 (count live))
          {:error :no-highlight :highlights (count live)}
          (let [{:keys [rows start end]} (block-around lines (first live))
                numbers (mapv :number rows)
                between (subvec lines (inc end) footer-i)
                tail-ok? (every? #(or (str/blank? %) (rule? %) (scroll-marker? %)
                                      (some? (option-line %))
                                      (> (indent %) (:col (first rows))))
                                 between)
                labels (mapv :label rows)]
            (cond
              (or (< (count rows) 2)
                  (not= numbers (vec (range (first numbers) (+ (first numbers) (count rows)))))
                  (not tail-ok?))
              {:error :not-a-menu}

              (some composer? between) {:error :composer-visible}

              (some #(re-matches checkbox-pattern %) labels) {:error :multi-select}

              :else
              (let [footer (str/trim (nth lines footer-i))
                    question (question-of lines start)]
                {:menu {:kind (kind-of footer)
                        :question (str/join "\n" question)
                        :options (mapv #(dissoc % :index :col) rows)
                        :highlighted (:number (first (filter :highlighted? rows)))
                        :footer footer
                        :fingerprint [(mapv normalized question) (mapv normalized labels)]}}))))))))

(defn select-option
  "Resolve OPTION, a one-based number or a label, against parsed MENU. Number
   means the printed option number, not the highlighted row. Text matches the
   full normalized label or the part before an em-dash description. Exactly
   one match is required. Returns {:option <row>} or a structured error map."
  [menu option]
  (let [s (some-> option str screen/utrim not-empty)
        number (when (and s (re-matches #"[1-9][0-9]?" s)) (Long/parseLong s))
        candidates (if number
                     (filterv #(= number (:number %)) (:options menu))
                     (let [wanted (normalized s)]
                       (filterv (fn [{:keys [label]}]
                                  (let [label (normalized label)
                                        short (normalized (first (str/split label #"\s+[—–]\s+" 2)))]
                                    (or (= wanted label) (= wanted short))))
                                (:options menu))))]
    (cond
      (nil? s) {:error :missing-option :hint "option=<number|label>"}
      (= 1 (count candidates)) {:option (first candidates)}
      (seq candidates) {:error :ambiguous-option :option option
                        :matches (mapv #(select-keys % [:number :label]) candidates)}
      :else {:error :no-such-option :option option
             :available (mapv #(select-keys % [:number :label]) (:options menu))})))

(defn- send-key!
  "Send one named KEY to PANE. Returns nil, or an error map naming the keys
   already delivered (SENT) when tmux refuses this one."
  [h pane key sent]
  (let [{:keys [exit err]} ((:tmux! h) ["send-keys" "-t" pane key])]
    (when-not (zero? exit)
      {:error :key-delivery-failed :pane pane :key key :exit exit :stderr err
       :keys-sent sent :delivered? false})))

(defn- send-arrows!
  "Send KEYS in order. Returns {:sent keys} or the failure of the first key
   tmux refused, with the keys sent before it."
  [h pane keys]
  (loop [[k & more] keys sent []]
    (if-not k
      {:sent sent}
      (or (send-key! h pane k sent) (recur more (conj sent k))))))

(defn- menu-view [menu]
  (select-keys menu [:kind :question :options :highlighted :footer]))

(defn- sending-job
  "The active job on PANE that is writing to its composer right now, or nil."
  [pane]
  (some #(when (and (= pane (:pane %)) (#{:queued :running} (:status %)) (:sending? %)) %)
        (compact/jobs)))

(def ^:private boilerplate-pattern
  "Question lines every dialog carries, which bind an expect to nothing."
  #"(?i)^\s*(?:do you want to|would you like to)\b.*\?\s*$")

(defn- dialog-body
  "MENU's question lines minus the generic `Do you want to ...?` prompt."
  [menu]
  (->> (str/split-lines (str (:question menu)))
       (remove #(re-matches boilerplate-pattern %))
       (str/join "\n")
       normalized))

(defn- expect-problem
  "Why EXPECT does not bind MENU, or nil: :expect-mismatch when it is not in
   the question, :expect-too-generic when it occurs only in the boilerplate."
  [menu expect]
  (let [wanted (normalized expect)]
    (cond
      (not (and (seq wanted) (str/includes? (normalized (:question menu)) wanted))) :expect-mismatch
      (not (str/includes? (dialog-body menu) wanted)) :expect-too-generic)))

(defn- gate
  "Why the SELECTED option of MENU must not be answered, or nil.
   :persistent-grant always refuses (\"don't ask again\", \"allow all ...
   this session\"). A permission dialog needs permission? AND an expect text
   found in its body; expect, when given, binds any menu to what the caller
   read."
  [menu selected {:keys [permission? expect]}]
  (let [refusal (fn [reason extra] (merge {:error reason :menu (menu-view menu)} extra))]
    (cond
      (or (re-find persistent-grant-pattern (:label selected))
          (and (= :permission-dialog (:kind menu)) (not (one-shot-label? (:label selected)))))
      (refusal :persistent-grant {:option (:number selected) :label (:label selected)
                                  :hint "a standing grant is the operator's alone; relay it with `swarm ask`"})

      (and (= :permission-dialog (:kind menu)) (not permission?))
      (refusal :permission-dialog {:hint (str "not positively an AskUserQuestion menu: relay the question and options"
                                              " to the operator with `swarm ask`, then answer with permission=true"
                                              " expect=<text from the dialog>")})

      (and permission? (str/blank? (str expect)))
      (refusal :expect-required {:hint "permission=true needs expect=<text that occurs in the dialog body>"})

      (some? expect)
      (when-let [problem (expect-problem menu expect)]
        (refusal problem {:expect expect
                          :hint "expect must quote the dialog's own body (the command, file or question), not its generic prompt"})))))


(defn audit-file
  "Where every answer attempt is appended, one EDN map per line:
   $XDG_STATE_HOME/hive-tmux/answers.edn, else ~/.local/state/hive-tmux/."
  []
  (let [base (or (not-empty (System/getenv "XDG_STATE_HOME"))
                 (str (System/getProperty "user.home") "/.local/state"))]
    (io/file base "hive-tmux" "answers.edn")))

(def ^:dynamic *audit-file*
  "A java.io.File overriding `audit-file`, or nil. Tests bind it."
  nil)

(defn append-audit!
  "Append RECORD to `audit-file`, durably (the file is flushed and closed)."
  [record]
  (let [^java.io.File f (or *audit-file* (audit-file))]
    (io/make-parents f)
    (spit f (str (pr-str record) "\n") :append true)))

(def ^:dynamic *audit!*
  "The audit sink `answer!` uses when OPTS carry no :audit!. Tests bind it."
  append-audit!)

(defn- audited
  "Record RESULT for ACTOR through AUDIT! and the pane's active job (when one
   exists); return RESULT with the job it was noted in."
  [h audit! actor pane menu selected result]
  (let [at ((:now-ms h))
        record (cond-> {:step :answer :actor actor :pane pane :at at
                        :outcome (or (:error result) (if (:confirmed? result) :confirmed :delivered))
                        :keys (vec (or (:keys result) (:keys-sent result) []))}
                 menu (assoc :kind (:kind menu) :question (:question menu))
                 selected (assoc :option (:number selected) :label (:label selected)))
        audit-error (try (audit! record) nil
                         (catch Exception e (or (.getMessage e) (str (class e)))))
        job (when (seq (:keys record)) (compact/record-step! pane record))]
    (cond-> (assoc result :pane pane :answered-by actor :answered-at at)
      audit-error (assoc :audit-failed true :audit-error audit-error)
      job (assoc :job-id (:job-id job) :job-status (:status job)))))

(defn- intent!
  "Audit, BEFORE any key, that ACTOR is about to answer SELECTED on PANE.
   nil when written, else the reason it could not be."
  [h audit! actor pane menu selected]
  (try (audit! {:step :answer-intent :actor actor :pane pane :at ((:now-ms h))
                :kind (:kind menu) :question (:question menu)
                :option (:number selected) :label (:label selected)})
       nil
       (catch Exception e (or (.getMessage e) (str (class e))))))

(defn- settle
  "After Enter: poll until the answered menu is gone. {:confirmed? true} when
   no menu shows, {:next-menu m} when a different one does, else
   {:confirmed? false} once TRIES captures still show the same menu."
  [h pane fingerprint {:keys [capture-lines confirm-tries confirm-delay-ms]}]
  (loop [n confirm-tries]
    (let [{:keys [menu]} (some-> (panes/capture h pane capture-lines) parse-menu)]
      (cond
        (and menu (not= fingerprint (:fingerprint menu))) {:confirmed? false :next-menu (menu-view menu)}
        (nil? menu) {:confirmed? true}
        (zero? n) {:confirmed? false}
        :else (do ((:sleep! h) confirm-delay-ms) (recur (dec n)))))))

(defn answer!
  "Answer the live menu on the pane ROW names, OPTION being its printed number
   or exact label. OPTS: :actor (recorded), :permission? and :expect (see
   `gate`), :audit! (fn of one record, default `append-audit!`) and :opts
   (timing overrides of `defaults`).

   Nothing is sent unless the screen parses as one live menu and the gate
   passes. The arrows go first; the pane is then read again, and Enter goes
   only when the same menu (question and labels) shows with the highlight on
   the chosen row, else :menu-changed with the keys already sent. Once Enter
   is delivered the result is :delivered? true, never a failure: :confirmed?
   says whether the menu closed and :next-menu shows a different menu that
   took its place. Every outcome is audited."
  ([h row option] (answer! h row option {}))
  ([h row option {:keys [actor opts audit!] :or {opts {}} :as gate-opts}]
   (let [cfg (merge defaults opts)
         audit! (or audit! *audit!*)
         pane (:pane row)
         parsed (some-> (panes/capture h pane (:capture-lines cfg)) parse-menu)
         menu (:menu parsed)
         choice (when menu (select-option menu option))
         selected (:option choice)
         refuse (fn [result] (audited h audit! actor pane menu selected (assoc result :delivered? false)))]
     (cond
       (nil? parsed) (refuse {:error :no-capture})
       (:error parsed) (refuse (dissoc parsed :menu))
       (:error choice) (refuse (assoc choice :menu (menu-view menu)))
       :else
       (or (some-> (gate menu selected gate-opts) refuse)
           (when-let [job (sending-job pane)]
             (refuse {:error :send-in-progress :job-id (:job-id job)}))
           (when-let [why (intent! h audit! actor pane menu selected)]
             (assoc (refuse {:error :audit-unavailable :audit-error why}) :keys-sent []))
           (let [delta (- (:number selected) (:highlighted menu))
                 arrows (vec (repeat (abs delta) (if (neg? delta) "Up" "Down")))
                 {:keys [sent] :as moved} (send-arrows! h pane arrows)]
             (if (:error moved)
               (refuse moved)
               (let [now (some-> (panes/capture h pane (:capture-lines cfg)) parse-menu :menu)]
                 (if-not (and now (= (:fingerprint menu) (:fingerprint now))
                              (= (:number selected) (:highlighted now)))
                   (refuse {:error :menu-changed :keys-sent sent
                            :now (some-> now menu-view)})
                   (if-let [failed (send-key! h pane "Enter" sent)]
                     (refuse failed)
                     (audited h audit! actor pane menu selected
                              (merge {:delivered? true :option (:number selected)
                                      :label (:label selected) :kind (:kind menu)
                                      :keys (conj sent "Enter")}
                                     (settle h pane (:fingerprint menu) cfg)))))))))))))

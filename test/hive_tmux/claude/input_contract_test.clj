(ns hive-tmux.claude.input-contract-test
  "Contract for the class of defect in review F1 (20261111111446): no reading
   of dim text may turn what could be a human's input into non-input. Over
   generated prompt boxes (any mix of typed and dim runs, extra box lines,
   voice marks, a missing closing rule or footer) every visible prompt
   character stays in :input unless it is proven ghost text."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-tmux.claude.screen :as screen]))

(def ^:private rule "────────────")

(def ^:private gen-word
  (gen/fmap #(apply str %) (gen/vector (gen/elements "abc xyz-/.") 1 6)))

(def ^:private gen-sgr
  (gen/elements ["2" "0" "22" "" "39" "38;5;2" "38;5;246;2" "1;2" "2;3" "0;2" "48;5;2"]))

(def ^:private gen-run
  (gen/let [sgr gen-sgr w gen-word] (str "\u001b[" sgr "m" w)))

(def ^:private gen-screen
  (gen/let [runs (gen/vector gen-run 0 5)
            box (gen/vector (gen/elements ["" "  more text" "  \u001b[2mdim line\u001b[0m"]) 0 2)
            closed? gen/boolean
            footer (gen/elements [nil "  -- INSERT --" "  listening…" "  \u001b[2mkeep holding…\u001b[0m"
                                  "  Voice: processing…" "  ● REC · tap to send"])]
    (let [prompt (str "\u001b[39m❯\u00a0" (apply str runs) "\u001b[0m")]
      {:prompt prompt
       :lines (vec (concat [rule prompt] box (when closed? [rule]) (when footer [footer])))})))

(defn- after-glyph [prompt]
  (subs prompt (inc (str/index-of prompt "❯"))))

(defn- voice? [lines]
  (some (fn [l] (some #(str/includes? (screen/strip-ansi l) %) screen/voice-marks)) lines))

(def no-possible-input-is-lost
  (prop/for-all [{:keys [prompt lines]} gen-screen]
    (let [{:keys [input suggestion]} (screen/read-screen lines)
          v (screen/utrim (screen/strip-ansi (after-glyph prompt)))
          dims (map second (filter #(seq (screen/utrim (first %)))
                                   (screen/sgr-runs (after-glyph prompt))))]
      (and
       ;; nothing visible at the prompt vanishes from both facts
       (or (str/blank? v) (some? input) (= v suggestion))
       ;; a suggestion is only ever all-dim text with no voice mark on screen
       (or (nil? suggestion)
           (and (nil? input) (= v suggestion) (every? true? dims) (not (voice? lines))))
       ;; typed (not dim) text is always input
       (or (str/blank? v) (every? true? dims) (some? input))
       ;; while dictation shows, any prompt text is input
       (or (str/blank? v) (not (voice? lines)) (some? input))))))

(deftest dim-text-never-hides-possible-human-input
  (let [r (tc/quick-check 500 no-possible-input-is-lost {:seed 20261111})]
    (is (:pass? r) (pr-str (select-keys r [:shrunk :seed])))))

(ns hive-tmux.claude.caller
  "Who is calling, and is a pane row the caller's own session? (card
   20261122175413-3d66aa77.)

   The host stamps every call with `_caller_id`. A coordinator is
   `coordinator:<token>`, and since bb-mcp #45 (2026-11-18) the token is the
   first 8 hex digits of the Claude session id. bb-mcp falls back to the
   client pid only when no session record was readable. The pid-only reader
   this replaces returned nil for a hex token, so every own-pane guard built
   on it FAILED OPEN: a session could compact its own pane or answer its own
   dialog.

   Ported from hive-self-steer's caller-token (PR #10). An 8-character token
   is always a session token, digits only included: a Linux pid has at most
   7 digits (pid_max 4194304).

   FAIL CLOSED. A `coordinator:` id whose token is neither form is
   :unresolvable. The guards refuse such a caller with a reason. They never
   treat it as \"not the caller's pane\".

   Stratum: Promote. Pure; no tmux, no files."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def prefix "coordinator:")

(def session-token-length
  "Hex digits of the Claude session id bb-mcp stamps as the caller token."
  8)

(defn- hex-token? [s]
  (and (string? s) (= session-token-length (count s))
       (every? #(str/index-of "0123456789abcdef" %) s)))

(defn- pid-token
  "The pid in TOKEN: 1 to 7 digits, positive. Nil otherwise."
  [token]
  (when (and (<= 1 (count token) 7) (every? #(Character/isDigit ^char %) token))
    (let [pid (parse-long token)] (when (pos? pid) pid))))

(defn caller-token
  "What a `coordinator:<token>` caller id names: {:session-token s} or
   {:pid n}, never both. Nil for anything else, including a coordinator id
   whose token is neither form (see `identity-of`)."
  [caller-id]
  (when (and (string? caller-id) (str/starts-with? caller-id prefix))
    (let [token (subs caller-id (count prefix))]
      (cond (hex-token? token) {:session-token token}
            (= session-token-length (count token)) nil
            :else (when-let [pid (pid-token token)] {:pid pid})))))

(defn identity-of
  "How a guard must treat CALLER-ID:
     {:token t}               a coordinator, `caller-token` T
     {:unresolvable id}       a `coordinator:` id that names nothing: REFUSE
     nil                      no coordinator caller (none, a ling, the
                              operator): no pane is its own"
  [caller-id]
  (let [id (some-> caller-id str str/trim not-empty)]
    (cond
      (nil? id) nil
      (not (str/starts-with? id prefix)) nil
      :else (if-let [t (caller-token id)] {:token t} {:unresolvable id}))))

(defn own-pane?
  "Does ROW show the session TOKEN (`caller-token`) names? A pid matches the
   row's :pid; a session token matches a row :session-id that starts with it.
   A bare number is read as a pid."
  [row token]
  (let [token (if (number? token) {:pid token} token)
        {:keys [pid session-token]} token]
    (boolean
     (or (and pid (number? (:pid row)) (= (long pid) (long (:pid row))))
         (and session-token (string? (:session-id row))
              (str/starts-with? (:session-id row) session-token))))))

(defn unresolvable-error
  "The refusal data of an :unresolvable caller."
  [id command]
  {:caller id :command command
   :why (str "the caller id " (pr-str id) " names neither a session (coordinator:<8 hex>) nor a pid, "
             "so its own pane cannot be told apart from another's; refused rather than risk acting on it")})

(ns hive-tmux.claude.ready
  "compact-ready (S2, card 20261122160542-14c8ef6c): when a Claude pane may be
   compacted without losing anything, and the one compactor that may act on it.

   A pane is compact-ready only when ALL hold:
     - its context is at or over the threshold;
     - it is idle (`compact/idle?`);
     - its prompt line is empty (:input nil): nobody is typing;
     - no dialog is open and the session is not :waiting (`compact/open-dialog?`);
     - its coordinator said it is done: it shouted WRAPPED, or a memory tagged
       handoff for its agent-session was updated, AFTER the context crossed
       the threshold.
   Evidence is always matched by the session (its id prefix or its pid, the
   two ways a coordinator is named), never by the pane: pane ids are reused
   across restarts (friction 181557dd).

   Strata:
     Promote   verdict / reseed-text / agent-ids / lease-verdict   pure
     Port      evidence (row -> {:wrapped-at :handoff}), lease {:read :write!},
               silence (row -> result)                             fns
     Boundary  host-evidence / file-lease / host-silencer          soft-resolved host fns, one file

   Exactly ONE compactor (decision 20261121215259-7b40b779): auto-compact
   holds a lease naming its owner; a second owner is refused while the lease
   is fresh."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-dsl.result :refer [guard]]
            [hive-tmux.claude.compact :as compact]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def reseed-template "hive workflow catchup /caveman, then read hand-off %s and resume")

(def lease-ttl-ms
  "A compactor lease not renewed for this long is free for another owner."
  (* 5 60 1000))

;;; Promote

(defn coordinator-tokens
  "The ids a coordinator of ROW's session goes by: the first 8 characters of
   its session id and its pid, as strings (`coordinator:<token>`, and the tag
   `agent-session:coordinator:<token>`)."
  [row]
  (let [sid (some-> (:session-id row) str)]
    (vec (distinct (remove str/blank?
                           [(when sid (subs sid 0 (min 8 (count sid))))
                            (some-> (:pid row) str)])))))

(defn agent-ids
  "The hivemind agent ids ROW's coordinator shouts as."
  [row]
  (mapv #(str "coordinator:" %) (coordinator-tokens row)))

(defn own-row?
  "Does ROW show the session of AGENT-ID (a `coordinator:<token>`)?"
  [row agent-id]
  (boolean (some #{(str agent-id)} (agent-ids row))))

(defn evidence-after
  "The part of EVIDENCE ({:wrapped-at ms, :handoff {:id :updated-ms}}) newer
   than CROSSED-AT: {:by :handoff :handoff-id id :at ms} (a hand-off names
   what to read, so it wins), {:by :wrapped :at ms}, or nil."
  [evidence crossed-at]
  (let [after? #(and (number? %) (number? crossed-at) (> % crossed-at))
        {:keys [id updated-ms]} (:handoff evidence)
        wrapped (:wrapped-at evidence)]
    (cond (after? updated-ms) (cond-> {:by :handoff :at updated-ms} id (assoc :handoff-id id))
          (after? wrapped) {:by :wrapped :at wrapped})))

(defn blockers
  "Why ROW is not compact-ready at THRESHOLD, its context having crossed it
   at CROSSED-AT, given EVIDENCE: a vector of keywords, empty when ready."
  [row threshold crossed-at evidence]
  (cond-> []
    (not (compact/due? row threshold)) (conj :under-threshold)
    (not (compact/idle? row)) (conj :busy)
    (:input row) (conj :human-typing)
    (compact/open-dialog? row) (conj :open-dialog)
    (nil? (:session-id row)) (conj :no-session)
    (nil? (evidence-after evidence crossed-at)) (conj :no-wrap-or-handoff)))

(defn verdict
  "{:ready? true :evidence e} when ROW is compact-ready (see the ns doc),
   else {:ready? false :blockers [...]}."
  [row threshold crossed-at evidence]
  (let [bs (blockers row threshold crossed-at evidence)]
    (if (empty? bs)
      {:ready? true :evidence (evidence-after evidence crossed-at)}
      {:ready? false :blockers bs})))

(defn candidate?
  "Does ROW pass every cheap condition, so only the coordinator's word is
   missing? Keeps the evidence port off busy, typing and waiting panes."
  [row threshold]
  (= [:no-wrap-or-handoff] (blockers row threshold 0 nil)))

(defn reseed-text
  "The re-seed after an auto-compact: catchup, then the hand-off it names."
  [{:keys [handoff-id]}]
  (format reseed-template (or handoff-id "(your newest, tagged handoff)")))

(defn lease-verdict
  "May OWNER hold the compactor lease LEASE ({:owner :renewed-ms} or nil) at
   NOW? {:ok? true}, or {:ok? false :holder owner :renewed-ms ms}. A lease
   not renewed for `lease-ttl-ms` is free; TAKEOVER? takes a fresh one."
  [lease owner now takeover?]
  (if (or (nil? lease) (= owner (:owner lease)) takeover?
          (not (number? (:renewed-ms lease)))
          (> (- now (:renewed-ms lease)) lease-ttl-ms))
    {:ok? true}
    {:ok? false :holder (:owner lease) :renewed-ms (:renewed-ms lease)}))

(defn ->ms
  "Epoch ms of X: a number, an inst, or an ISO-8601 (zoned) string; else nil."
  [x]
  (cond (number? x) (long x)
        (inst? x) (inst-ms x)
        (string? x) (let [s (str/replace x #"\[[^\]]*\]$" "")]
                      (or (guard Exception nil (.toEpochMilli (.toInstant (java.time.OffsetDateTime/parse s))))
                          (guard Exception nil (.toEpochMilli (java.time.Instant/parse s)))))))

(defn- message-ms [m]
  (some #(some-> (get m %) ->ms) [:timestamp :ts :at :created :time]))

(defn wrapped-at
  "The newest time one of MESSAGES (a coordinator's hivemind rows) says
   WRAPPED, or nil."
  [messages]
  (let [ms (keep #(when (some (fn [v] (and (string? v) (str/includes? v "WRAPPED"))) (vals %))
                    (message-ms %))
                 messages)]
    (when (seq ms) (reduce max ms))))

(defn newest-handoff
  "The newest of ENTRIES (memory rows tagged handoff for the session):
   {:id :updated-ms}, or nil."
  [entries]
  (some->> entries
           (keep (fn [e] (when-let [ms (->ms (or (:updated e) (:created e)))] {:id (:id e) :updated-ms ms})))
           seq (apply max-key :updated-ms)))

;;; Boundary

(defn- soft [sym] (guard Throwable nil (requiring-resolve sym)))

(defn host-evidence
  "The evidence port over the host, resolved softly: a fn of ROW ->
   {:wrapped-at ms :handoff {:id :updated-ms}} (keys absent when unknown).
   WRAPPED is read from the hivemind messages of each of the session's agent
   ids, the hand-off from memory rows tagged handoff and
   agent-session:coordinator:<token>."
  []
  (let [messages (soft 'hive-mcp.hivemind.status/get-agent-messages)
        query (soft 'hive-mcp.vectordb.facade/query-entries)]
    (fn [row]
      (let [ws (when messages (keep #(guard Throwable nil (wrapped-at (messages %))) (agent-ids row)))
            hs (when query
                 (keep #(guard Throwable nil
                          (newest-handoff (query :tags ["handoff" (str "agent-session:coordinator:" %)] :limit 20)))
                       (coordinator-tokens row)))]
        (cond-> {}
          (seq ws) (assoc :wrapped-at (reduce max ws))
          (seq hs) (assoc :handoff (apply max-key :updated-ms hs)))))))

(defn lease-file []
  (io/file (System/getProperty "user.home") ".local" "state" "hive-tmux" "compactor.edn"))

(defn file-lease
  "The lease port over one EDN file (default `lease-file`), shared by every
   process on the host: {:read (fn [] lease-or-nil) :write! (fn [lease-or-nil])}."
  ([] (file-lease (lease-file)))
  ([f]
   {:read (fn [] (guard Exception nil (edn/read-string (slurp f))))
    :write! (fn [lease]
              (if lease
                (do (io/make-parents f) (spit f (pr-str lease)))
                (guard Exception nil (io/delete-file f true))))}))

(def silence-sym
  "A host fn that silences a session's own event wakers (its sixth-sense
   monitor, its short awaits), {:agent-ids [...] :session-id s} -> result.
   Resolved softly; absent, only this watcher's own shouts to that
   coordinator are held (`hive-tmux.claude.watch`)."
  'hive-agent.sixth-sense.api/silence-wakers!)

(defn host-silencer
  "A fn of ROW that silences its session's wakers through the host, or nil."
  []
  (when-let [f (soft silence-sym)]
    (fn [row] (f {:agent-ids (agent-ids row) :session-id (:session-id row)}))))

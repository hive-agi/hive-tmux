(ns hive-tmux.observe.socket-path
  "Where the observe socket lives (STD-04 T2), as a pure rule the ClojureWasm
   hook client can load.

   Rule:
     $XDG_RUNTIME_DIR/hive/tmux-observe.sock        when XDG_RUNTIME_DIR is set
     <tmpdir>/hive-<uid>/tmux-observe.sock          otherwise
   and the path is at most `max-path-bytes` UTF-8 bytes.

   This is a COPY of hive-system.network.path (hive-system 0.4.4), which the
   JVM listener calls. The hook client cannot: ClojureWasm resolves only
   `:git/url` and `:local/root` coordinates, so like `wire` this namespace
   requires nothing outside clojure.core and clojure.string. Results are the
   plain maps hive-dsl uses ({:ok v} or {:error category ...}), so the two
   copies compare directly. They cannot drift unnoticed:
   test/hive_tmux/observe/socket_path_test.clj runs both on the shared
   vectors in socket_path_vectors.edn and on generated inputs.

   Effects (reading the environment and the uid) stay with the caller."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def service
  "The T2 service name of the observe socket."
  "tmux-observe")

(def max-path-bytes
  "Longest socket path, in UTF-8 bytes, the rule accepts."
  100)

(def ^:private name-re
  "A service or id: letters, digits, dot, underscore, dash; no leading dot."
  #"[A-Za-z0-9_\-][A-Za-z0-9._\-]*")

(defn- ok [v] {:ok v})

(defn- err [category data] (merge {:error category} data))

(defn utf8-length
  "Number of bytes `s` takes in UTF-8."
  [s]
  (alength (.getBytes ^String s "UTF-8")))

(defn- blank->nil [s]
  (when-not (str/blank? s) s))

(defn- strip-trailing-slash [s]
  (if (and (> (count s) 1) (str/ends-with? s "/"))
    (recur (subs s 0 (dec (count s))))
    s))

(defn socket-dir
  "The directory hive sockets of this user live in. `env` maps environment
   variable names to values; `tmpdir` is java.io.tmpdir; `uid` the numeric
   user id. => {:ok dir} or {:error ...}"
  [env tmpdir uid]
  (if-let [xdg (blank->nil (get env "XDG_RUNTIME_DIR"))]
    (ok (str (strip-trailing-slash xdg) "/hive"))
    (cond
      (str/blank? tmpdir)
      (err :socket-path/no-base {:message "neither XDG_RUNTIME_DIR nor tmpdir is set"})

      (nil? uid)
      (err :socket-path/no-uid {:message "uid is required when XDG_RUNTIME_DIR is unset"})

      :else
      (ok (str (strip-trailing-slash tmpdir) "/hive-" uid)))))

(defn socket-file-name
  "<service>[-<id>].sock, after checking both names. => {:ok name} or {:error ...}"
  ([svc] (socket-file-name svc nil))
  ([svc id]
   (let [svc (some-> svc str)
         id  (some-> id str)]
     (cond
       (or (nil? svc) (not (re-matches name-re svc)))
       (err :socket-path/bad-service {:service svc
                                      :message "service must match [A-Za-z0-9_-][A-Za-z0-9._-]*"})

       (and (some? id) (not (re-matches name-re id)))
       (err :socket-path/bad-id {:id id
                                 :message "id must match [A-Za-z0-9_-][A-Za-z0-9._-]*"})

       :else
       (ok (str svc (when id (str "-" id)) ".sock"))))))

(defn socket-path
  "The socket path of `svc` (and optional `id`) under the T2 rule; refuses a
   path longer than `max-path-bytes` UTF-8 bytes.
   => {:ok {:path :dir}} or {:error ...}"
  ([env tmpdir uid svc] (socket-path env tmpdir uid svc nil))
  ([env tmpdir uid svc id]
   (let [dir  (socket-dir env tmpdir uid)
         file (socket-file-name svc id)]
     (cond
       (contains? dir :error)  dir
       (contains? file :error) file
       :else
       (let [path  (str (:ok dir) "/" (:ok file))
             bytes (utf8-length path)]
         (if (> bytes max-path-bytes)
           (err :socket-path/too-long {:path path :bytes bytes :max max-path-bytes
                                       :message (str "socket path is " bytes " bytes, the limit is "
                                                     max-path-bytes)})
           (ok {:path path :dir (:ok dir)})))))))

(defn proc-status-uid
  "The real uid in the text of /proc/self/status (its `Uid:` line), or nil.
   The hook client's only source of the uid: ClojureWasm has no getuid."
  [text]
  (when text
    (some (fn [line]
            (when (str/starts-with? line "Uid:")
              (some-> (re-find #"\d+" line) parse-long)))
          (str/split-lines text))))

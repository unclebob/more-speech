(ns more-speech.db.xtdb
  (:require [more-speech.db.gateway :as gateway]
            [clojure.java.io :as io]
            [xtdb.api :as xt]))

(defmethod gateway/add-profile ::type [db id profile]
  )

(defmethod gateway/get-profile ::type [db id]
  )

(defmethod gateway/event-exists? ::type [db id]
  )

(defmethod gateway/add-event ::type [db id event]
  )

(defmethod gateway/get-event ::type [db id]
  )

(defmethod gateway/update-event-as-read ::type [db id]
  )

(defmethod gateway/add-relays-to-event ::type [db id relays]
  )

(defmethod gateway/add-reference-to-event ::type [db id reference]
  )

(defmethod gateway/add-contacts ::type [db user-id contacts]
  )

(defmethod gateway/get-contacts ::type [db user-id]
  )

(defmethod gateway/get-id-from-petname ::type [db user-id petname]
  )

(defmethod gateway/get-id-from-username ::type [db user-name]
  )

;--------- XTDB utilities

(defn start-xtdb! []
  (letfn [(kv-store [dir]
            {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                        :db-dir (io/file dir)
                        :sync? true}})]
    (xt/start-node
      {:xtdb/tx-log (kv-store "data/dev/tx-log")
       :xtdb/document-store (kv-store "data/dev/doc-store")
       :xtdb/index-store (kv-store "data/dev/index-store")})))

(def xtdb-db (atom nil))

(defn get-db []
  (if @xtdb-db
    @xtdb-db
    (reset! xtdb-db {:node (start-xtdb!) ::gateway/type ::type})))
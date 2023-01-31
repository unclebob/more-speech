(ns more-speech.db.xtdb
  (:require [more-speech.db.gateway :as gateway]
            [clojure.java.io :as io]
            [xtdb.api :as xt]))

(defn add-entity [db type id entity]
  (let [node (:node db)
        tx (xt/submit-tx
             node
             [[::xt/put
               (assoc entity :xt/id {:type type :id id})]])]
    (xt/await-tx node tx)))

(defn get-entity [db type id]
  (dissoc
    (xt/entity (xt/db (:node db)) {:type type :id id})
    :xt/id))

(defn delete-entity [db type id]
  (let [node (:node db)
        tx (xt/submit-tx node [[::xt/evict {:type type :id id}]])]
    (xt/await-tx node tx)))

(defmethod gateway/add-profile ::type [db id profile]
  (add-entity db :profile id profile))

(defmethod gateway/get-profile ::type [db id]
  (get-entity db :profile id))

(defn delete-profile [db id]
  (delete-entity db :profile id))

(defmethod gateway/event-exists? ::type [db id]
  (some? (gateway/get-event db id)))

(defmethod gateway/add-event ::type [db id event]
  (add-entity db :event id event))

(defmethod gateway/get-event ::type [db id]
  (get-entity db :event id))

(defn delete-event [db id]
  (delete-entity db :event id))

(defmethod gateway/update-event-as-read ::type [db id]
  (let [node (:node db)
        tx (xt/submit-tx node [[::xt/fn :assoc-entity
                                {:type :event :id id}
                                :read true]])]
    (xt/await-tx node tx))
  )

(defmethod gateway/add-relays-to-event ::type [db id relays]
  (let [node (:node db)
        tx (xt/submit-tx node [[::xt/fn :update-relays
                                {:type :event :id id}
                                relays]])]
    (xt/await-tx node tx)))

(defmethod gateway/add-reference-to-event ::type [db id reference]
  (let [node (:node db)
        tx (xt/submit-tx node [[::xt/fn :add-reference
                                {:type :event :id id}
                                reference]])]
    (xt/await-tx node tx))
  )

(defmethod gateway/add-contacts ::type [db user-id contacts]
  (add-entity db :contacts user-id {:contacts contacts}))

(defmethod gateway/get-contacts ::type [db user-id]
  (:contacts (get-entity db :contacts user-id)))

(defn delete-contacts [db user-id]
  (delete-entity db :contacts user-id))

(defmethod gateway/get-id-from-username ::type [db user-name]
  (let [node (:node db)
        result (xt/q (xt/db node)
                     '{:find [id]
                       :where [[profile :name user-name]
                               [profile :xt/id id]]})
        [{:keys [id]}] (first result)]
        id))

;--------- XTDB utilities

(defn add-assoc-entity [node]
  (xt/submit-tx
    node
    [[::xt/put
      {:xt/id :assoc-entity
       :xt/fn '(fn [ctx eid key val]
                 (let [db (xtdb.api/db ctx)
                       entity (xtdb.api/entity db eid)]
                   [[::xt/put (assoc entity key val)]]))}]]))

(defn add-update-entity [node]
  (xt/submit-tx
    node
    [[::xt/put
      {:xt/id :update-entity
       :xt/fn '(fn [ctx eid key f val]
                 (let [db (xtdb.api/db ctx)
                       entity (xtdb.api/entity db eid)]
                   [[::xt/put (update entity key f val)]]))}]]))

(defn add-update-relays [node]
  (xt/submit-tx
    node
    [[::xt/put
      {:xt/id :update-relays
       :xt/fn '(fn [ctx eid relays]
                 (let [db (xtdb.api/db ctx)
                       entity (xtdb.api/entity db eid)
                       old-relays (:relays entity)
                       new-relays (set (concat old-relays relays))]
                   [[::xt/put (assoc entity :relays new-relays)]]))}]]))

(defn add-add-reference [node]
  (xt/submit-tx
    node
    [[::xt/put
      {:xt/id :add-reference
       :xt/fn '(fn [ctx eid reference]
                 (let [db (xtdb.api/db ctx)
                       entity (xtdb.api/entity db eid)
                       old-references (:references entity)]
                   [[::xt/put (update entity :references conj reference)]]))}]]))

(defn add-procedures [node]
  (add-assoc-entity node)
  (add-update-entity node)
  (add-update-relays node)
  (add-add-reference node)
  )

(defn start-xtdb! [directory]
  (letfn [(kv-store [dir]
            {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                        :db-dir (io/file dir)
                        :sync? true}})]
    (let [node (xt/start-node
                 {:xtdb/tx-log (kv-store (str directory "/data/dev/tx-log"))
                  :xtdb/document-store (kv-store (str directory "/data/dev/doc-store"))
                  :xtdb/index-store (kv-store (str directory "/data/dev/index-store"))})]
      (add-procedures node)
      node)))

(def xtdb-db (atom nil))

(defn stop! []
  (when @xtdb-db
    (.close (:node @xtdb-db))
    (reset! xtdb-db nil)))

(defn get-db [directory]
  (if @xtdb-db
    @xtdb-db
    (reset! xtdb-db {:node (start-xtdb! directory) ::gateway/type ::type})))
(ns more-speech.db.xtdb
  (:require [clojure.java.io :as io]
            [more-speech.db.gateway :as gateway]
            [more-speech.nostr.util :as util]
            [xtdb.api :as xt]))

(defn sync-db [db]
  (xt/sync (:node db)))

(defn add-entity [db type id entity]
  (let [node (:node db)]
    (xt/submit-tx
      node
      [[::xt/put
        (assoc entity :xt/id {:type type :id (bigint id)})]])
    ))

(defn make-profile-transaction [id profile]
  (let [id (bigint id)]
    [::xt/put (assoc profile :xt/id {:type :profile :id id})]))

(defn make-profile-transactions [profiles-map]
  (loop [ids (keys profiles-map)
         transactions []]
    (if (empty? ids)
      transactions
      (let [id (first ids)
            profile (get profiles-map id)]
        (recur (rest ids) (conj transactions (make-profile-transaction id profile)))))))

(defmethod gateway/add-profiles-map ::type [db profiles-map]
  (let [node (:node db)
        profile-transactions (make-profile-transactions profiles-map)
        tx (xt/submit-tx node profile-transactions)]
    (xt/await-tx node tx)))

(defn make-one-contacts-transaction [id contacts]
  (let [id (bigint id)]
    [::xt/put {:contacts contacts :xt/id {:type :contacts :id id}}]))

(defn fix-contacts [contacts]
  (for [contact contacts]
    (if (nil? (:pubkey contact))
      contact
      (update contact :pubkey bigint))))

(defn make-contacts-transactions [contacts-map]
  (loop [ids (keys contacts-map)
         transactions []]
    (if (empty? ids)
      transactions
      (let [id (first ids)
            contacts (get contacts-map id)
            fixed-contacts (fix-contacts contacts)]
        (recur (rest ids) (conj transactions (make-one-contacts-transaction id fixed-contacts)))))))

(defmethod gateway/add-contacts-map ::type [db contacts-map]
  (let [node (:node db)
        transaction (make-contacts-transactions contacts-map)
        tx (xt/submit-tx node transaction)]
    (xt/await-tx node tx)))

(defn get-entity [db type id]
  (dissoc
    (xt/entity (xt/db (:node db)) {:type type :id (bigint id)})
    :xt/id))

(defn delete-entity [db type id]
  (let [node (:node db)
        tx (xt/submit-tx node [[::xt/evict {:type type :id (bigint id)}]])]
    (xt/await-tx node tx)))

(defmethod gateway/add-profile ::type [db id profile]
  (add-entity db :profile id profile))

(defmethod gateway/get-profile ::type [db id]
  (get-entity db :profile id))

(defn delete-profile [db id]
  (delete-entity db :profile id))

(defmethod gateway/event-exists? ::type [db id]
  (some? (gateway/get-event db id)))

(defmethod gateway/add-event ::type [db event]
  (let [id (:id event)
        event (assoc event :id (bigint id))]
    (add-entity db :event id event)))

(defmethod gateway/get-event ::type [db id]
  (get-entity db :event id))

(defn make-event-transaction [event]
  (let [id (bigint (:id event))]
    [::xt/put (assoc event :id id :xt/id {:type :event :id id})]))

(defmethod gateway/add-events ::type [db events]
  (let [event-transactions (map make-event-transaction events)
        node (:node db)
        tx (xt/submit-tx node event-transactions)]
    (xt/await-tx node tx)))

(defmethod gateway/get-event-ids-since ::type [db start-time]
  (let [node (:node db)
        result (xt/q (xt/db node)
                     '{:find [id event-time]
                       :in [start-time]
                       :where [[event :created-at event-time]
                               [event :id id]
                               [(>= event-time start-time)]]
                       :order-by [[event-time :desc]]}
                     start-time)]
    (map first result))
  )

(defmethod gateway/get-ids-of-read-events-since ::type [db start-time]
  (let [node (:node db)
        result (xt/q (xt/db node)
                     '{:find [id event-time]
                       :in [start-time]
                       :where [[event :created-at event-time]
                               [event :id id]
                               [event :read true]
                               [(>= event-time start-time)]]
                       :order-by [[event-time :desc]]}
                     start-time)]
    (map first result)))

(defmethod gateway/get-some-recent-event-authors ::type [db after]
  (let [node (:node db)
        result (xt/q (xt/db node)
                     '{:find [author]
                       :in [after]
                       :where [[e :xt/id txid]
                               [(get txid :type) type]
                               [e :pubkey author]
                               [e :created-at t]
                               [(= :event type)]
                               [(> t after)]]
                       :limit 2000}
                     after)]
    (set (map first result))))

(defmethod gateway/get-ids-by-author-since ::type [db author start-time]
  (let [node (:node db)
        author (bigint author)
        result (xt/q (xt/db node)
                     '{:find [id event-time]
                       :in [start-time author]
                       :where [[event :created-at event-time]
                               [event :id id]
                               [event :pubkey author]
                               [(>= event-time start-time)]]
                       :order-by [[event-time :desc]]}
                     start-time author)]
    (map first result)))

(defn cites? [id tags]
  (let [id (util/hexify id)]
    (some #(= id (second %)) tags)))

(defmethod gateway/get-ids-that-cite-since ::type [db target-id start-time]
  (let [ids (gateway/get-event-ids-since db start-time)]
    (filter #(cites? target-id (:tags (gateway/get-event db %))) ids)))

(defn delete-event [db id]
  (delete-entity db :event id))

(defmethod gateway/update-event-as-read ::type [db id]
  (let [node (:node db)
        tx (xt/submit-tx node [[::xt/fn :assoc-entity
                                {:type :event :id (bigint id)}
                                :read true]])]
    (xt/await-tx node tx))
  )

(defmethod gateway/add-relays-to-event ::type [db id relays]
  (let [node (:node db)
        tx (xt/submit-tx node [[::xt/fn :update-relays
                                {:type :event :id (bigint id)}
                                relays]])]
    (xt/await-tx node tx)))

(defmethod gateway/add-reference-to-event ::type [db id reference]
  (let [node (:node db)
        tx (xt/submit-tx node [[::xt/fn :add-reference
                                {:type :event :id (bigint id)}
                                reference]])]
    (xt/await-tx node tx))
  )

(defmethod gateway/add-reaction ::type [db id pubkey content]
  (let [node (:node db)
        tx (xt/submit-tx node [[::xt/fn :add-reaction
                                {:type :event :id (bigint id)}
                                pubkey content]])]
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
                       :in [user-name]
                       :where [[profile :name user-name]
                               [profile :xt/id id]]}
                     user-name)
        [{:keys [id]}] (first result)]
    id))

(defmethod gateway/get-profiles-after ::type [db after]
  (let [node (:node db)
        result (xt/q (xt/db node)
                     '{:find [id name]
                       :in [after]
                       :where [[e :xt/id txid]
                               [(get txid :id) id]
                               [(get txid :type) type]
                               [e :name name]
                               [e :created-at t]
                               [(= :profile type)]
                               [(> t after)]]}
                     after)]
    result))

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

(defn add-add-reaction [node]
  (xt/submit-tx
    node
    [[::xt/put
      {:xt/id :add-reaction
       :xt/fn '(fn [ctx eid pubkey content]
                 (let [conj-set (fn [s v] (conj (set s) v))
                       db (xtdb.api/db ctx)
                       entity (xtdb.api/entity db eid)
                       old-reactions (:reactions entity)]
                   [[::xt/put (update entity :reactions conj-set [pubkey content])]]))}]]))

(defn add-procedures [node]
  (add-assoc-entity node)
  (add-update-entity node)
  (add-update-relays node)
  (add-add-reference node)
  (add-add-reaction node)
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
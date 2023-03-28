(ns more-speech.nostr.tab-searcher
  (:use [seesaw core font tree])
  (:require [more-speech.logger.default :refer [log-pr]]
            [more-speech.nostr.util :as util]
            [more-speech.mem :refer :all]
            [more-speech.config :refer [get-db]]
            [more-speech.ui.formatters :as formatters]
            [more-speech.bech32 :as bech32]
            [more-speech.db.gateway :as gateway]
            [more-speech.nostr.contact-list :as contact-list]
            [more-speech.ui.swing.article-tree-util :as at-util]
            [more-speech.config :as config]))

(defn get-subject [event]
  (let [subject-tags (filter #(= :subject (first %)) (:tags event))]
    (second (first subject-tags))))

(defn id-matches-tag [id tags]
  (some #(= (util/hexify id) (second %)) tags))

(defn match-id [id event]
  (or (= id (:pubkey event))
      (= id (:id event))
      (id-matches-tag id (:tags event))))

(defn match-hex-id [target event]
  (let [id (util/unhexify target)]
    (match-id id event)))

(defn match-string-target [target event]
  (let [content (formatters/replace-references event)
        author-name (:name (gateway/get-profile (get-db) (:pubkey event)))
        author-id (contact-list/get-pubkey-from-petname target)
        author-id (or author-id (gateway/get-id-from-username (get-db) target))
        petname (contact-list/get-petname (:pubkey event))
        subject (get-subject event)
        re-target (re-pattern target)]
    (or (re-find re-target content)
        (and subject (re-find re-target subject))
        (and author-name (re-find re-target author-name))
        (and petname (re-find re-target petname))
        (and author-id (match-id author-id event)))))

(defn match-npub [target event]
  (match-id (bech32/address->number target) event))

(defn is-npub? [target]
  (try
    (bech32/address->number target)
    true
    (catch Exception _e
      false)))

(defn match-target [target event]
  (cond
    (is-npub? target)
    (match-npub target event)

    (re-matches #"\#\".*\"" target)
    (match-string-target (read-string target) event)

    (re-matches config/hex-key-pattern target)
    (match-hex-id target event)

    :else
    (match-string-target target event)))

(defn get-search-target [tab-name]
  (let [search-id (keyword (str "#" tab-name "-search"))
        search-text (select (get-mem :frame) [search-id])
        target-text (config search-text :text)]
    target-text))

(defn get-status-field [tab-name]
  (let [status-id (keyword (str "#" tab-name "-status"))
        status-field (select (get-mem :frame) [status-id])]
    status-field))

(defn set-search-status [tab-name status]
  (let [status-field (get-status-field tab-name)]
    (config! status-field :text status)))

(defn event-matches [target id]
  (let [event (gateway/get-event (get-db) id)]
    (match-target target event)))

(defn add-id-to-results [[target n results] id]
  [target n (conj results id)])

(defn update-search-status [tab-name]
  (let [[_ n results] (get-mem [:tab-search tab-name])]
    (set-search-status tab-name (format "%d of %d" n (count results)))))

(defn inc-n [[target n results]]
  (if (zero? (count results))
    [target n results]
    (let [next-n (if (= n (count results)) 1 (inc n))]
      [target next-n results])))

(defn dec-n [[target n results]]
  (if (zero? (count results))
    [target n results]
    (let [next-n (if (<= n 1) (count results) (dec n))]
      [target next-n results])))

(defn update-position-and-select [tab-name update-f]
  (when (some? (get-mem [:tab-search tab-name]))
    (let [[_ n results] (update-mem [:tab-search tab-name] update-f)]
      (when (> (count results) 0)
        (update-search-status tab-name)
        (let [event-id (nth results (dec n))
              tree (get-mem [:tab-tree-map tab-name])
              model (config tree :model)
              root-node (.getRoot model)
              node (at-util/find-header-node root-node event-id)]
          (at-util/select-tree-node tree node))))))

(defn select-next [tab-name _e]
  (update-position-and-select tab-name inc-n))

(defn select-prev [tab-name _e]
  (update-position-and-select tab-name dec-n))

(defn load-tab-search [tab-name]
  (let [[target] (get-mem [:tab-search tab-name])
        tree (get-mem [:tab-tree-map tab-name])
        model (config tree :model)
        root (.getRoot model)]
    (try
      (loop [children (enumeration-seq (.children root))
             n 0]
        (if (= target (get-search-target tab-name))
          (let [child (first children)]
            (cond
              (nil? child)
              (update-search-status tab-name)

              (event-matches target (.getUserObject child))
              (do
                (update-mem [:tab-search tab-name]
                            add-id-to-results (.getUserObject child))
                (update-search-status tab-name)
                (when (zero? n)
                  (select-next tab-name nil))
                (recur (rest children) (inc n)))

              :else
              (recur (rest children) n)))))
      (catch Exception e
        (log-pr 1 'load-tab-search e)))))

(defn search-event [tab-name e]
  (let [c (.getKeyChar e)]
    (when (= \newline c)
      (set-search-status tab-name "")
      (let [target-text (get-search-target tab-name)]
        (set-mem [:tab-search tab-name] [target-text 0 []])
        (when-not (empty? target-text)
          (set-search-status tab-name "Searching...")
          (future (load-tab-search tab-name)))))))

(ns more-speech.nostr.trust-updater
  (:require
    [more-speech.config :as config]
    [more-speech.config :refer [get-db]]
    [more-speech.db.gateway :as gateway]
    [more-speech.mem :refer :all]
    [more-speech.nostr.event-composers :as composers]
    [more-speech.nostr.util :as util]
    [more-speech.ui.formatter-util :as f-util]
    )
  (:use [seesaw.core]))

(defn untrust [his-pubkey]
  (let [my-pubkey (get-mem :pubkey)
        my-contacts (gateway/get-contacts (get-db) my-pubkey)
        reduced-contacts (remove #(= his-pubkey (:pubkey %)) my-contacts)]
    (gateway/add-contacts (get-db) my-pubkey reduced-contacts)
    (gateway/sync-db (get-db)))
  )

(defn entrust [his-pubkey his-petname]
  (let [my-pubkey (get-mem :pubkey)
        my-contacts (gateway/get-contacts (get-db) my-pubkey)
        his-entry (first (filter #(= his-pubkey (:pubkey %)) my-contacts))
        everyone-elses-entries (remove #(= his-pubkey (:pubkey %)) my-contacts)
        new-entry (if (some? his-entry)
                    (assoc his-entry :petname his-petname)
                    {:pubkey his-pubkey :petname his-petname})
        my-updated-contacts (conj everyone-elses-entries new-entry)]
    (gateway/add-contacts (get-db) my-pubkey my-updated-contacts)
    (gateway/sync-db (get-db))
    my-updated-contacts))

(defn entrust-and-send [his-pubkey his-petname]
  (let [my-contact-list (entrust his-pubkey his-petname)]
    (composers/compose-and-send-contact-list my-contact-list)))

(defn ask-for-petname [pubkey]
  (loop [profile (gateway/get-profile (get-db) pubkey)]
    (let [petname (input "Name this author"
                         :value (:name profile)
                         :title (str "Entrust " (f-util/abbreviate (util/hexify pubkey) 10)))]
      (cond
        (nil? petname)
        nil

        (re-matches config/user-name-chars petname)
        petname

        :else
        (do (alert (str "Invalid pet-name: " petname))
            (recur profile))))))

(defn trust-this-author [id]
  (let [petname (ask-for-petname id)]
    (when (some? petname)
      (entrust-and-send id petname))))

(defn trust-author-of-this-event [event _e]
  (let [id (:pubkey event)]
    (trust-this-author id)))

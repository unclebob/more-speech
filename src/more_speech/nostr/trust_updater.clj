(ns more-speech.nostr.trust-updater
  (:require [more-speech.mem :refer :all]
            [more-speech.nostr.event-composers :as composers]
            [more-speech.db.gateway :as gateway]
            [more-speech.config :refer [get-db]]))

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
    my-updated-contacts))

(defn entrust-and-send [his-pubkey his-petname]
  (let [my-contact-list (entrust his-pubkey his-petname)]
  (composers/compose-and-send-contact-list my-contact-list)))
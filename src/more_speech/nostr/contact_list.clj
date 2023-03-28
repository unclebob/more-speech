(ns more-speech.nostr.contact-list
  (:require [more-speech.nostr.util :as util]
            [more-speech.logger.default :refer [log-pr]]
            [more-speech.mem :refer :all]
            [more-speech.db.gateway :as gateway]
            [more-speech.bech32 :as bech32]
            [more-speech.config :refer [get-db]]
            [more-speech.config :as config]))


(defn make-contact-from-tag [[_p pubkey relay petname]]
  (try
    (let [pubkey (if (re-matches config/hex-key-pattern pubkey)
                   (util/hex-string->num pubkey)
                   (bech32/address->number (.trim pubkey)))]
      {:pubkey pubkey :relay relay :petname petname})
    (catch Exception e
      (log-pr 2 'make-contact-from-tag 'exception [pubkey relay petname] (.getMessage e))
      nil)))

(defn unpack-contact-list-event [event]
  (let [pubkey (:pubkey event)
        tags (:tags event)
        ptags (filter #(= :p (first %)) tags)
        contacts (map make-contact-from-tag ptags)
        contacts (remove nil? contacts)]
    [pubkey contacts]))

(defn process-contact-list [db event]
  (let [[pubkey contacts] (unpack-contact-list-event event)]
    (when (seq contacts)
      (gateway/add-contacts db pubkey contacts))))

(defn is-trusted? [candidate-pubkey]
  (let [my-pubkey (get-mem :pubkey)
        my-contacts (gateway/get-contacts (get-db) my-pubkey)
        my-contact-pubkeys (set (map :pubkey my-contacts))]
    (or (= candidate-pubkey my-pubkey)
        (contains? my-contact-pubkeys candidate-pubkey))))

(defn which-contact-trusts [candidate-pubkey]
  (let [my-pubkey (get-mem :pubkey)
        my-contacts (gateway/get-contacts (get-db) my-pubkey)]
    (loop [my-contact-ids (map :pubkey my-contacts)]
      (if (empty? my-contact-ids)
        nil
        (let [my-contact (first my-contact-ids)
              his-contacts (gateway/get-contacts (get-db) my-contact)
              his-contact-ids (set (map :pubkey his-contacts))]
          (if (contains? his-contact-ids candidate-pubkey)
            my-contact
            (recur (rest my-contact-ids))))))))

(defn get-petname [his-pubkey]
  (let [my-pubkey (get-mem :pubkey)
        my-contacts (gateway/get-contacts (get-db) my-pubkey)
        his-entry (first (filter #(= his-pubkey (:pubkey %)) my-contacts))]
    (:petname his-entry)))

(defn get-pubkey-from-petname [petname]
  (loop [contacts (gateway/get-contacts (get-db) (get-mem :pubkey))]
    (if (seq contacts)
      (if (= petname (:petname (first contacts)))
        (:pubkey (first contacts))
        (recur (rest contacts)))
      nil)))

(defn get-trustees
  ([]
   (get-trustees (get-mem :pubkey)))
  ([id]
   (let [contacts (gateway/get-contacts (get-db) id)]
     (set (concat [(get-mem :pubkey)]
             (map :pubkey contacts))))))

(defn get-web-of-trust
  ([]
   (get-web-of-trust (get-mem :pubkey)))
  ([id]
   (let [trustees (get-trustees id)
         trusted-of-trustees (mapcat get-trustees trustees)]
     (set (concat [(get-mem :pubkey)]
                  trustees
                  trusted-of-trustees)))))


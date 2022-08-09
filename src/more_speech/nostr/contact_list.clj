(ns more-speech.nostr.contact-list
  (:require [more-speech.nostr.util :as util]
            [more-speech.ui.swing.ui-context :refer :all]))


(defn make-contact-from-tag [[_p pubkey relay petname]]
  (try
    {:pubkey (util/hex-string->num pubkey) :relay relay :petname petname}
    (catch Exception e
      (prn 'make-contact-from-tag 'exception [pubkey relay petname] (.getMessage e))
      nil))
  )

(defn unpack-contact-list-event [event]
  (let [pubkey (:pubkey event)
        tags (:tags event)
        ptags (filter #(= :p (first %)) tags)
        contacts (map make-contact-from-tag ptags)
        contacts (remove nil? contacts)]
    [pubkey contacts]))

(defn process-contact-list [event-state event _url]
  (let [[pubkey contacts] (unpack-contact-list-event event)]
    (if (seq contacts)
      (assoc-in event-state [:contact-lists pubkey] contacts)
      event-state))
  )

(defn is-trusted? [candidate-pubkey]
  (let [event-state @(:event-context @ui-context)
        my-pubkey (:pubkey event-state)
        contact-lists (:contact-lists event-state)
        my-contacts (get contact-lists my-pubkey)
        my-contact-pubkeys (set (map :pubkey my-contacts))]
    (or (= candidate-pubkey my-pubkey)
        (contains? my-contact-pubkeys candidate-pubkey))))

(defn trusted-by-contact [candidate-pubkey]
  (let [event-state @(:event-context @ui-context)
          my-pubkey (:pubkey event-state)
          contact-lists (:contact-lists event-state)
          my-contacts (get contact-lists my-pubkey)
          my-contact-ids (map :pubkey my-contacts)]
    (loop [my-contact-ids my-contact-ids]
      (if (empty? my-contact-ids)
        nil
        (let [my-contact (first my-contact-ids)
              his-contacts (set (map :pubkey (get contact-lists my-contact)))]
          (if (contains? his-contacts candidate-pubkey)
            my-contact
            (recur (rest my-contact-ids)))))))
  )

(defn get-petname [his-pubkey]
  (let [event-state @(:event-context @ui-context)
        my-pubkey (:pubkey event-state)
        contact-lists (:contact-lists event-state)
        my-contacts (get contact-lists my-pubkey)
        his-entry (first (filter #(= his-pubkey (:pubkey %)) my-contacts))]
    (:petname his-entry)))

(defn get-pubkey-from-petname [petname]
  (let [event-state @(:event-context @ui-context)
          my-pubkey (:pubkey event-state)
          contact-lists (:contact-lists event-state)
          my-contacts (get contact-lists my-pubkey)]
    (loop [contacts my-contacts]
      (if (seq contacts)
        (if (= petname (:petname (first contacts)))
          (:pubkey (first contacts))
          (recur (rest contacts)))
        nil))
      ) )




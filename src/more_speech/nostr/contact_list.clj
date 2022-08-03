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

(defn get-petname [his-pubkey]
  (let [event-state @(:event-context @ui-context)
        my-pubkey (:pubkey event-state)
        contact-lists (:contact-lists event-state)
        my-contacts (get contact-lists my-pubkey)
        his-entry (first (filter #(= his-pubkey (:pubkey %)) my-contacts))]
    (:petname his-entry)))




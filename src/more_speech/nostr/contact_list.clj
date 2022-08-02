(ns more-speech.nostr.contact-list
  (:require [more-speech.nostr.util :as util]))


(defn make-contact-from-tag [[_p pubkey relay petname]]
  {:pubkey (util/hex-string->num pubkey) :relay relay :petname petname}
  )

(defn unpack-contact-list-event [event]
  (let [pubkey (:pubkey event)
        tags (:tags event)
        ptags (filter #(= :p (first %)) tags)]
    [pubkey (map make-contact-from-tag ptags)]))

(defn process-contact-list [event-state event _url]
  (let [[pubkey contacts] (unpack-contact-list-event event)]
    (assoc-in event-state [:contact-lists pubkey] contacts))
  )


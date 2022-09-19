(ns more-speech.nostr.trust-updater
  (:require [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.nostr.events :as events]))

(defn entrust [his-pubkey his-petname]
  (let [event-context (:event-context @ui-context)
        my-pubkey (:pubkey @event-context)
        contact-lists (:contact-lists @event-context)
        my-contacts (get contact-lists my-pubkey)
        his-entry (first (filter #(= his-pubkey (:pubkey %)) my-contacts))
        everyone-elses-entries (remove #(= his-pubkey (:pubkey %)) my-contacts)
        new-entry (if (some? his-entry)
                    (assoc his-entry :petname his-petname)
                    {:pubkey his-pubkey :petname his-petname})
        my-updated-contacts (conj everyone-elses-entries new-entry)]
    (swap! event-context assoc-in [:contact-lists my-pubkey] my-updated-contacts)
    my-updated-contacts))

(defn entrust-and-send [his-pubkey his-petname]
  (let [my-contact-list (entrust his-pubkey his-petname)]
  (events/compose-and-send-contact-list my-contact-list)))
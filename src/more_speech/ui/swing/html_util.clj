(ns more-speech.ui.swing.html-util
  (:require
    [more-speech.ui.swing.user-info-interface :as html-interface]
    [clojure.java.browse :as browse]
    [clojure.string :as string]
    [more-speech.bech32 :as bech32]
    [more-speech.config :refer [get-db]]
    [more-speech.db.gateway :as gateway]
    [more-speech.logger.default :refer [log-pr]]
    [more-speech.nostr.contact-list :as contact-list]
    [more-speech.nostr.event-composers :as composers]
    [more-speech.nostr.protocol :as protocol]
    [more-speech.nostr.util :as util]
    [more-speech.ui.swing.util :as swing-util]
    [seesaw.mouse :as mouse])
  (:use (seesaw [border] [core]))
  (:import (javax.swing.event HyperlinkEvent$EventType)))

(defn get-user-id-from-subject [subject]
  (try
    (cond
      (.startsWith subject "@")
      (composers/find-user-id (subs subject 1))

      (or (.startsWith subject "npub1")
          (.startsWith subject "nprofile1"))
      (bech32/address->number subject)

      :else
      (do
        (let [petname-id (contact-list/get-pubkey-from-petname subject)]
          (if (some? petname-id)
            petname-id
            (gateway/get-id-from-username (get-db) subject)))
        ))
    (catch Exception _e
      nil)))

(defn get-user-info [id _e]
  (html-interface/show-user-profile id))

(defn pop-up-name-menu [e subject]
  (let [id (get-user-id-from-subject subject)
        profile (gateway/get-profile (get-db) id)
        p (popup :items [(action :name "Get Info..."
                                 :handler (partial get-user-info id))])
        ev (.getInputEvent e)
        [x y] (mouse/location ev)]
    (if (some? profile)
      (swing-util/show-popup p e x y)
      (protocol/request-profiles-and-contacts-for id))))

(defn open-link [e]
  (when (= HyperlinkEvent$EventType/ACTIVATED (.getEventType e))
    (when-let [url (str (.getURL e))]
      (let [[type subject] (string/split (.getDescription e) #"\:\/\/")]
        (cond
          (or (= type "http") (= type "https"))
          (try
            (browse/browse-url url)
            (catch Exception ex
              (log-pr 1 'open-link url (.getMessage ex))
              (log-pr 1 ex)))

          (= type "ms-idreference")
          (let [id (util/unhexify subject)]
            (protocol/request-note id)
            (protocol/request-profiles-and-contacts-for id)
            (swing-util/select-event id))

          (= type "ms-notereference")
          (try
            (let [id (bech32/address->number subject)]
              (protocol/request-note id)
              (swing-util/select-event id))
            (catch Exception ex
              (log-pr 1 'open-link url (.getMessage ex))))

          (= type "ms-neventreference")
          (try
            (let [tlv (bech32/address->tlv subject)
                  hex-id (:special tlv)
                  id (util/unhexify hex-id)]
              (protocol/request-note id)
              (swing-util/select-event id))
            (catch Exception e
              (log-pr 1 'open-link url (.getMessage e))))

          (= type "ms-namereference")
          (pop-up-name-menu e subject)

          :else
          (do (log-pr 1 'open-link url 'type type 'subject subject)
              (log-pr 1 (.getDescription e)))
          )))))

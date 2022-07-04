;;Notes:
;; Nice debug site: https://nostr-army-knife.netlify.app

(ns more-speech.core
  (:gen-class)
  (:require [clojure.core.async :as async]
            [more-speech.config :as config]
            [more-speech.nostr.protocol :as protocol]
            [more-speech.ui.swing.main-window :as swing]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.relays :as relays]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.migrator :as migrator])
  )

(def send-chan (async/chan))

(declare more-speech
         setup-jframe
         set-event-handler
         read-old-events)

(defn ^:export -main [& _args]
  (migrator/migrate-to config/migration-level)
  (let [keys (read-string (slurp @config/keys-filename))
        _ (relays/load-relays-from-file @config/relays-filename)
        read-event-ids (read-string (slurp @config/read-event-ids-filename))
        nicknames (read-string (slurp @config/nicknames-filename))
        profiles (read-string (slurp @config/profiles-filename))
        tabs (read-string (slurp @config/tabs-filename))
        event-context (events/make-event-context {:keys keys
                                                  :send-chan send-chan
                                                  :nicknames nicknames
                                                  :profiles profiles
                                                  :read-event-ids read-event-ids
                                                  :tabs tabs
                                                  })
        _ (swap! ui-context assoc :event-context event-context)
        handler (swing/setup-main-window)
        ]
    (swap! event-context set-event-handler handler)
    (let [latest-old-message-time (read-old-events event-context handler)]
      (protocol/get-events event-context latest-old-message-time))
    (spit @config/nicknames-filename
          (with-out-str
            (clojure.pprint/pprint (:nicknames @event-context))))
    (spit @config/profiles-filename
          (with-out-str
            (clojure.pprint/pprint (:profiles @event-context))))
    (spit @config/read-event-ids-filename
          (with-out-str
            (clojure.pprint/pprint (:read-event-ids @event-context))))
    (spit @config/relays-filename
          (with-out-str
            (clojure.pprint/pprint (relays/relays-for-writing))))
    (spit @config/messages-filename
          (with-out-str
            (clojure.pprint/pprint (:text-event-map @event-context))))

    (System/exit 1)))

(defn set-event-handler [event-state handler]
  (assoc event-state :event-handler handler))

(defn read-old-events [event-context handler]
  (let [old-events (vals (read-string (slurp @config/messages-filename)))]
    (doseq [event old-events]
      (let [url (first (:relays event))]
        (swap! event-context events/add-event event url)
        (events/handle-text-event handler event))
      )
    (apply max (map :created-at old-events))))



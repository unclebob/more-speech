;;Notes:
;; Nice debug site: https://nostr-army-knife.netlify.app

(ns more-speech.core
  (:require [clojure.core.async :as async]
            [more-speech.nostr.protocol :as protocol]
            [more-speech.ui.swing.main-window :as swing]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.relays :as relays]
            [more-speech.ui.swing.ui-context :refer :all])
  )

(def send-chan (async/chan))

(declare more-speech
         setup-jframe
         set-event-handler)

(defn ^:export -main [& _args]
  (let [keys (read-string (slurp "private/keys"))
        _ (relays/load-relays-from-file "private/relays")
        read-event-ids (read-string (slurp "private/read-event-ids"))
        nicknames (read-string (slurp "private/nicknames"))
        tabs (read-string (slurp "private/tabs"))
        event-context (events/make-event-context {:keys keys
                                                  :send-chan send-chan
                                                  :nicknames nicknames
                                                  :read-event-ids read-event-ids
                                                  :tabs tabs
                                                  })
        _ (swap! ui-context assoc :event-context event-context)
        handler (swing/setup-main-window)
        ]
    (swap! event-context set-event-handler handler)
    (protocol/get-events event-context)
    (spit "private/nicknames"
          (with-out-str
            (clojure.pprint/pprint (:nicknames @event-context))))
    (spit "private/read-event-ids"
          (with-out-str
            (clojure.pprint/pprint (:read-event-ids @event-context))))
    (spit "private/relays"
          (with-out-str
            (clojure.pprint/pprint (relays/relays-for-writing))))
    (System/exit 1)))

  (defn set-event-handler [event-state handler]
    (assoc event-state :event-handler handler))





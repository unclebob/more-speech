;;Stories
;; - Add author/date, etc. to replies.
;; - Start checking sdefs in update.
;; - Clean up java schnorr library.
;; - Threading needs some rethinking.
;; - Mark read and highlight properly.
;; - Save names and headers.  Request after latest save.
;; - Consider subject/topic in the tags

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
        event-agent (events/make-event-agent {:keys keys
                                              :send-chan send-chan
                                              :nicknames nicknames
                                              :read-event-ids read-event-ids
                                              :tabs tabs
                                              })
        _ (swap! ui-context assoc :event-agent event-agent)
        handler (swing/setup-main-window)
        ]
    (send event-agent set-event-handler handler)
    (await event-agent) ; wait for the agent to complete.
    (protocol/get-events event-agent)
    (spit "private/nicknames" (:nicknames @event-agent))
    (spit "private/read-event-ids" (:read-event-ids @event-agent)))
  (System/exit 1)
  )

(defn set-event-handler [event-state handler]
  (assoc event-state :event-handler handler))




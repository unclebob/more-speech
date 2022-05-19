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
            [more-speech.nostr.relays :as relays])
  )

(def send-chan (async/chan))

(declare more-speech
         setup-jframe
         set-event-handler)

(defn ^:export -main [& _args]
  (let [keys (read-string (slurp "private/keys"))
        _ (relays/load-relays-from-file "private/relays")
        event-agent (events/make-event-agent keys send-chan)
        handler (swing/setup-main-window event-agent)
        ]
    (send event-agent set-event-handler handler)
    (protocol/get-events event-agent))
  (System/exit 1)
  )

(defn set-event-handler [event-state handler]
  (assoc event-state :event-handler handler))




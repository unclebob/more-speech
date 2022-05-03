;;Stories
;; - p tags for text events
;; - validate incoming messages.
;; - Add author/date, etc. to replies.
;; - Start checking sdefs in update.
;; - Clean up java schnorr library.
;; - Threading does not work quite right.  Do some diagnosis.
;; - Mark read and highlight properly.
;; - Save names and headers.  Request after latest save.
;; - Consider subject/topic in the tags

;;Notes:
;; Nice debug site: https://nostr-army-knife.netlify.app

(ns more-speech.core
  (:require [clojure.core.async :as async]
            [more-speech.nostr.protocol :as protocol]
            [more-speech.ui.swing.main-window :as swing]
            [more-speech.nostr.events :as events])
  )

(def send-chan (async/chan))

(defn get-keys [state]
  (let [keys (read-string (slurp "private/keys"))
        state (assoc state :keys keys)]
    state))

(declare more-speech
         setup-jframe
         set-event-handler)

(defn ^:export -main [& _args]
  (let [keys (read-string (slurp "private/keys"))
        event-agent (events/make-event-agent keys send-chan)
        handler (swing/setup-jframe event-agent)
        ]
    (send event-agent set-event-handler handler)
    (protocol/get-events event-agent))
  (System/exit 1)
  )

(defn set-event-handler [event-state handler]
  (assoc event-state :event-handler handler))




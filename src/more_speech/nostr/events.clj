(ns more-speech.nostr.events
  (:require [more-speech.article :as article]
            [clojure.data.json :as json]))

(defn process-event [{:keys [application] :as state} event]
  (let [{:keys [articles nicknames]} application
        name-of (fn [pubkey] (get nicknames pubkey pubkey))
        [name subscription-id inner-event :as decoded-msg] event
        {:strs [id pubkey created_at kind tags content sig]} inner-event]
    (condp = kind
      0 (update-in
          state [:application :nicknames]
          assoc pubkey (get (json/read-str content) "name" "tilt"))
      3 (do (printf "%s: %s %s %s\n" kind (article/format-time created_at) (name-of pubkey) content)
            state)
      1 (assoc-in state [:application :articles]
                  (conj articles
                        (article/make-article (name-of pubkey) created_at content)))
      4 (do (printf "%s: %s %s %s\n" kind (article/format-time created_at) (name-of pubkey) content)
            state)
      (do (prn "unknown event: " event)
          state)
      )))
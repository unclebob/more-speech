(ns more-speech.data-storage
  (:require [more-speech.config :as config]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.relays :as relays]
            [more-speech.ui.swing.tabs :as tabs]))


(defn write-configuration []
  (let [event-context (:event-context @ui-context)]
    (spit @config/profiles-filename
          (with-out-str
            (clojure.pprint/pprint (:profiles @event-context))))
    (spit @config/read-event-ids-filename
          (with-out-str
            (clojure.pprint/pprint (:read-event-ids @event-context))))
    (spit @config/relays-filename
          (with-out-str
            (clojure.pprint/pprint (relays/relays-for-writing))))))

(defn write-messages []
  (let [event-context (:event-context @ui-context)]
    (spit @config/messages-filename
          (with-out-str
            (clojure.pprint/pprint (:text-event-map @event-context))))))

(defn load-configuration []
  (let [keys (read-string (slurp @config/keys-filename))
        read-event-ids (read-string (slurp @config/read-event-ids-filename))
        profiles (read-string (slurp @config/profiles-filename))
        tabs-list (tabs/ensure-tab-list-has-all
                    (read-string (slurp @config/tabs-list-filename)))
        event-context (events/make-event-context {:keys keys
                                                  :profiles profiles
                                                  :read-event-ids read-event-ids
                                                  :tabs-list tabs-list
                                                  })]
    (swap! ui-context assoc :event-context event-context)
    (relays/load-relays-from-file @config/relays-filename)))

(defn read-old-events [event-context handler]
  (let [old-events (vals (read-string (slurp @config/messages-filename)))
        creation-times (map :created-at old-events)]
    (doseq [event old-events]
      (let [url (first (:relays event))]
        (swap! event-context events/add-event event url)
        (events/handle-text-event handler event)))
    (if (empty? creation-times)
      (-> (System/currentTimeMillis) (quot 1000) (- 86400))
      (apply max creation-times))))

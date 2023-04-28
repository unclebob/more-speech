(ns more-speech.nostr.protocol
  (:require [more-speech.config :as config]
            [more-speech.logger.default :refer [log-pr]]
            [more-speech.mem :refer :all]
            [more-speech.mem :refer :all]
            [more-speech.nostr.contact-list :as contact-list]
            [more-speech.nostr.event-dispatcher :as handlers]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.util :as util]
            [more-speech.relay :as relay]
            [more-speech.websocket-relay :as ws-relay])
  (:import (java.util Date)
           (java.text SimpleDateFormat)
           (java.util Timer TimerTask)))

(defn- format-time [time]
  (let [time (* time 1000)
        date (Date. (long time))]
    (.format (SimpleDateFormat. "MM/dd/yyyy kk:mm:ss z") date)))

(defn send-request [[_type id _filters :as request]]
  (doseq [url (keys @relays)]
    (when (not= :read-none (get-in @relays [url :read]))
      (let [relay (:connection (get @relays url))]
        (when (some? relay)
          (set-mem [:active-subscriptions url id] {:eose :close})
          (relay/send relay request))))))

(defn- make-request-id []
  (let [r (rand-int 1000000)]
    (str "ms-request-" r)))

(defn request-note [id]
  (let [req-id (make-request-id)
        request ["REQ" req-id {"kinds" [1] "ids" [(util/hexify id)]}]]
    (send-request request))
  )

(defn request-profiles-and-contacts-for [authors]
  (let [authors (if (coll? authors) authors [authors])
        hexified-authors (map util/hexify authors)
        trimmed-authors (if (<= (count hexified-authors) 100)
                          hexified-authors
                          (map #(subs % 0 10) (take 1000 (shuffle hexified-authors))))
        r (rand-int 1000000)
        request ["REQ" (str "ms-request-" r) {"kinds" [0 3]
                                              "authors" trimmed-authors}]]
    (send-request request)
    ))

(defn request-contact-lists [relay]
  (let [now (quot (System/currentTimeMillis) 1000)
        days-ago config/read-contact-lists-days-ago
        seconds-ago (int (* days-ago 86400))
        since (int (- now seconds-ago))]
    (relay/send relay ["REQ" "ms-contacts" {"kinds" [3] "since" since}])))

(defn request-metadata [relay since]
  (relay/send relay ["REQ" "ms-profiles" {"kinds" [0] "since" since}]))

(defn add-trustees [type filters who]
  (if (some? who)
    (let [selected-keys (set (take 999 (shuffle who)))]
      (assoc filters type selected-keys))
    filters))

(defn request-batch [url id back-to filter]
  (let [relay (get-in @relays [url :connection])
        since (- (util/get-now) config/batch-time)
        until (util/get-now)
        query (assoc filter "since" since
                            "until" until
                            "limit" config/batch-size)]
    (set-mem [:active-subscriptions url id]
             {:eose :next-batch
              :min-time (util/get-now) :max-time 0 :event-counter 0
              :back-to back-to :since since :until until :filter filter})
    (relay/send relay ["REQ" id query])))

(defn request-next-batch [url id]
  (let [relay (get-in @relays [url :connection])
        {:keys [min-time event-counter back-to
                filter since]} (get-mem [:active-subscriptions url id])
        new-since (if (or (zero? event-counter)
                          (< (- min-time since)
                             (quot config/batch-time 2)))
                    (- since config/batch-time)
                    since)
        until (if (zero? event-counter) since min-time)
        query (assoc filter "since" new-since
                            "until" until
                            "limit" config/batch-size)]
    (cond
      (< min-time back-to)
      (do
        (prn 'min-time min-time 'back-to back-to)
        (update-mem [:active-subscriptions "url"] dissoc id)
        (relay/send relay ["CLOSE" id]))

      :else
      (do
        (update-mem [:active-subscriptions url id]
                    assoc :since new-since :until until :max-time 0 :event-counter 0)
        (relay/send relay ["REQ" id query])))))

(defn send-subscription
  ([relay since now]
   (send-subscription relay since now nil))

  ([relay since now who]
   (let [kind-filter {"kinds" [0 1 2 3 4 7 9735]}
         past-filter (merge kind-filter {"since" since "until" now})
         future-filter (merge kind-filter {"since" now})
         short-who (map #(subs % 0 10) who)
         trustees (contact-list/get-trustees)
         trustees (if (empty? trustees) [] (map util/hexify trustees))
         past-author-filter (add-trustees "authors" past-filter short-who)
         future-author-filter (add-trustees "authors" future-filter short-who)
         past-mention-filter (add-trustees "#p" past-filter trustees)
         future-mention-filter (add-trustees "#p" future-filter trustees)
         url (::ws-relay/url relay)]
     (when (> now since)
       (if (some? who)
         (do (request-batch url "ms-past-author" since past-author-filter)
             (request-batch url "ms-past-mention" since past-mention-filter))
         (request-batch url "ms-past" since past-filter)))
     (if (some? who)
       (relay/send relay ["REQ" "ms-future" future-author-filter future-mention-filter])
       (relay/send relay ["REQ" "ms-future" future-filter])))))

(defn subscribe-all
  ([relay]
   (let [now (util/get-now)
         since (- now 86400)]
     (send-subscription relay since now)))
  ([relay since now]
   (send-subscription relay since now)))

(defn subscribe-to-pubkeys [relay since now pubkeys]
  (let [trustees (map util/hexify pubkeys)]
    (send-subscription relay since now trustees)))

(defn subscribe-trusted [relay since now]
  (subscribe-to-pubkeys relay since now (contact-list/get-trustees)))

(defn subscribe-web-of-trust [relay since now]
  (subscribe-to-pubkeys relay since now (contact-list/get-web-of-trust)))

(defn unsubscribe [relay]
  (relay/send relay ["CLOSE" "ms-past"])
  (relay/send relay ["CLOSE" "ms-future"])
  )

(defn close-relay [relay]
  (relay/close relay)
  (swap! relays assoc-in [(::ws-relay/url relay) :connection] nil))

(defn add-event-time [[earliest latest] time]
  (let [earliest (if (nil? earliest) time (min earliest time))
        latest (if (nil? latest) time (max latest time))]
    [earliest latest]))

(defn close-subscription [relay url id]
  (relay/send relay ["CLOSE" id])
  (update-mem [:active-subscriptions url] dissoc id))

(defn count-event [active-subscription event]
  (let [{:keys [event-counter min-time max-time]} active-subscription
        created-at (get event "created_at")
        min-time (if (nil? min-time) created-at min-time)
        max-time (if (nil? max-time) created-at max-time)
        event-counter (if (nil? event-counter) 0 event-counter)
        min-time (min created-at min-time)
        max-time (max created-at max-time)
        event-counter (inc event-counter)]
    (assoc active-subscription
      :min-time min-time
      :max-time max-time
      :event-counter event-counter)))

(defn handle-relay-message [url message]
  (let [relay (:connection (get @relays url))
        [type id event] message]

    (cond
      (= type "EVENT")                                      ;nobody uses this right now...
      (update-mem [:active-subscriptions url id] count-event event)

      (= type "EOSE")
      (let [active-subscription (get-mem [:active-subscriptions url id])]
        (prn "EOSE" url id)
        (when (some? active-subscription)
          (prn 'active-subscription url id active-subscription)
          (condp = (:eose active-subscription)
            :close (close-subscription relay url id)
            :next-batch (request-next-batch url id)
            nil))))
    (update-mem :websocket-backlog inc)
    (update-mem :incoming-events inc)
    (update-mem [:events-by-relay url] #(if (nil? %) 1 (inc %)))
    (send-off events/event-agent handlers/handle-event message url)))

(defn subscribe-to-relay [url since now]
  (let [relay (get-in @relays [url :connection])
        read-type (get-in @relays [url :read])]
    (when (and (or (not= :false read-type)
                   (not= :no-read read-type))
               (some? relay))
      (condp = read-type
        true (subscribe-all relay since now)
        :read-all (subscribe-all relay since now)
        :read-trusted (subscribe-trusted relay since now)
        :read-web-of-trust (subscribe-web-of-trust relay since now)
        nil)
      (swap! relays assoc-in [url :subscribed] true))))

(defn subscribe-to-relays [subscription-time now]
  (let [date (- subscription-time 3600)]
    (log-pr 1 'subscription-date date (format-time date))
    (doseq [url (keys @relays)]
      (subscribe-to-relay url date now))))

(defn connect-to-relay [relay]
  (let [url (::ws-relay/url relay)
        relay-config (get @relays url)
        read-type (:read relay-config)
        readable? (or (= read-type true)
                      (= read-type :read-all)
                      (= read-type :read-trusted)
                      (= read-type :read-web-of-trust))
        writeable? (:write relay-config)
        should-connect? (or readable? writeable?)
        open-relay (if should-connect?
                     (relay/open relay)
                     nil)]
    (when (some? open-relay)
      (swap! relays assoc-in [url :connection] open-relay))))

(defn increment-relay-retry
  "used in a swap!, increments retries counter unless the last retry was over an hour ago
  and then resets to 1"
  [relays url]
  (let [relay (get relays url)
        retries (get relay :retries 0)
        last-retry-time (get relay :retry-time 0)
        now (util/get-now-ms)
        time-since-last-retry (- now last-retry-time)
        retries (if (> time-since-last-retry 3600000) 1 (inc retries))
        relays (assoc-in relays [url :retries] retries)
        relays (assoc-in relays [url :retry-time] now)]
    relays))

(defn handle-close [relay]
  (let [url (::ws-relay/url relay)]
    (swap! relays assoc-in [url :connection] nil)
    (log-pr 1 url 'is-closed)))

(defn make-relay [url]
  (ws-relay/make url {:recv handle-relay-message
                      :close handle-close}))

(defn reconnect-to-relay [url since now]
  (log-pr 1 'reconnecting-to url)
  (connect-to-relay (make-relay url))
  (subscribe-to-relay url since now))

(defn retry-relay [url since]
  (let [now (util/get-now)
        retrying? (get-in @relays [url :retrying])]
    (when (not retrying?)
      (log-pr 1 'relay-closed url)
      (swap! relays assoc-in [url :retrying] true)
      (swap! relays assoc-in [url :connection] nil)
      (future
        (let [retries (get-in @relays [url :retries] 0)
              seconds-to-wait (min 300 (* retries 30))]
          (log-pr 1 'retries retries url)
          (log-pr 1 'waiting seconds-to-wait 'seconds url)
          (swap! relays increment-relay-retry url)
          (Thread/sleep (* 1000 seconds-to-wait))
          (swap! relays assoc-in [url :retrying] false)
          (reconnect-to-relay url since now))))))

(defn is-dead? [url]
  (let [now (util/get-now)
        deadman (get-mem [:deadman url])
        deadman (if (nil? deadman) now deadman)
        dead-time (- now deadman)]
    (> dead-time 120)))

(defn check-open [url]
  (let [relay (get-in @relays [url :connection])]
    (when-not (get-in @relays [url :retrying])
      (when (is-dead? url)
        (log-pr 1 'relay-check-open-deadman-timeout url)
        (when (some? relay)
          (relay/close relay))
        (future (retry-relay url (get-mem [:deadman url])))))))

(defn is-active-url? [url]
  (let [relay-descriptor (get @relays url)
        read-state (:read relay-descriptor)
        write-state (:write relay-descriptor)
        is-reading? (or (= :read-all read-state)
                        (= :read-trusted read-state)
                        (= :read-web-of-trust read-state))
        is-writing? write-state]
    (or is-reading? is-writing?)))

(defn check-all-active-relays []
  (doseq [url (keys @relays)]
    (when (is-active-url? url)
      (check-open url))))

(defn connect-to-relays []
  (doseq [url (keys @relays)]
    (let [relay (make-relay url)]
      (connect-to-relay relay)))
  (log-pr 1 'relay-connection-attempts-complete))

(defn request-contact-lists-from-relays []
  (log-pr 1 'requesting-contact-lists)
  (doseq [url (keys @relays)]
    (let [relay (get-in @relays [url :connection])
          read-type (get-in @relays [url :read])]
      (when (= :read-all read-type)
        (request-contact-lists relay)))))

(defn request-metadata-from-relays [since]
  (log-pr 1 'requesting-metadata)
  (doseq [url (keys @relays)]
    (let [relay (get-in @relays [url :connection])
          read? (get-in @relays [url :read])]
      (when (and read? (some? relay))
        (request-metadata relay since)))))

(defn close-all-relays []
  (doseq [url (keys @relays)]
    (let [relay (get-in @relays [url :connection])]
      (when (some? relay)
        (relay/close relay)))))

(defn reconnect-all-relays []
  (let [now (- (util/get-now) 120)]
    (close-all-relays)
    (Thread/sleep 1000)
    (doseq [url (keys @relays)]
      (reconnect-to-relay url now now))))


(defn initialize []
  (let [timer (Timer. "Dead socket timer")
        check-open-task (proxy [TimerTask] []
                          (run [] (check-all-active-relays)))
        s60 (long 60000)]
    (doseq [kind [0 1 2 3 4 7 9735]]
      (set-mem [:event-counter :kinds kind] 0))
    (.schedule timer check-open-task s60 s60)))



(ns more-speech.nostr.relays
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            (more-speech
              [config :as config]
              [relay :as relay]
              [websocket-relay :as ws-relay])
            [more-speech.logger.default :refer [log-pr]]
            [more-speech.mem :refer :all]))

(defn- connection? [c]
  (= (::relay/type c) ::ws-relay/websocket))
(s/def ::read #{:read-all :read-trusted :read-web-of-trust :read-none})
(s/def ::write boolean?)
(s/def ::retries integer?)
(s/def ::retrying boolean?)
(s/def ::subscribed boolean?)
(s/def ::connection (s/nilable connection?))
(s/def ::relay (s/keys :req-un [::read ::write ::subscribed ::connection]
                       :req-opt [::retries ::retrying]))
(s/def ::relays (s/map-of string? ::relay))

(defn set-relay-defaults [relays]
  (loop [urls (keys relays)
         relays relays]
    (if (empty? urls)
      relays
      (let [url (first urls)
            relay (get relays url)
            relay (assoc relay :connection nil :subscribed false)]
        (recur (rest urls) (assoc relays url relay))))))

(defn load-relays [relay-text]
  (let [loaded-relays (if (empty? relay-text)
                        {}
                        (read-string relay-text))
        loaded-relays (set-relay-defaults loaded-relays)]
    (set-mem :relays loaded-relays)))

(defn load-relays-from-file [file-name]
  (load-relays (slurp file-name)))

(defn relays-for-reading []
  (let [relays (get-mem :relays)]
    (loop [urls (keys relays)
           relays-to-read []]
      (if (empty? urls)
        relays-to-read
        (let [url (first urls)
              relay (get relays url)
              read (:read relay)
              read (condp = read false :read-none true :read-all read)]
          (if (not= :read-none read)
            (recur (rest urls) (conj relays-to-read url))
            (recur (rest urls) relays-to-read)))))))

(defn relays-for-writing []
  (let [relays (get-mem :relays)]
    (loop [urls (keys relays)
           relays-to-write {}]
      (if (empty? urls)
        relays-to-write
        (let [url (first urls)
              relay (get relays url)
              read (:read relay)
              read (condp = read false :read-none true :read-all read)
              write (:write relay)]
          (recur (rest urls)
                 (assoc relays-to-write url {:read read :write write})))))))

(defn validate-relay-url [url]
  (if (empty? url)
    nil
    (let [lurl (.trim (.toLowerCase url))]
      (if (and (re-matches config/relay-pattern lurl)
               (= -1 (.indexOf lurl "localhost")))
        (if (.endsWith lurl "/")
          (subs lurl 0 (dec (count lurl)))
          lurl)
        nil))))

(defn add-relay [url]
  (let [checked-url (validate-relay-url url)]
    (when (and (not (empty? url))
               (empty? checked-url))
      (log-pr 2 'invalid-relay url))
    (when (and (not (empty? checked-url))
               (not (contains? (get-mem :relays) checked-url)))
      (log-pr 2 'adding-relay checked-url)
      (set-mem [:relays checked-url] {:read :read-none :write false}))))

(defn add-recommended-relays-in-tags [event]
  (loop [tags (:tags event)]
    (if (empty? tags)
      nil
      (let [[tag-type _ url] (first tags)]
        (when (or (= tag-type :e) (= tag-type :p))
          (add-relay url)
          (recur (rest tags)))))))

(defn get-domain-name [url]
  (try
    (let [[_protocol uri] (string/split url #"://")
          [domain-name _args] (string/split uri #"/")]
      domain-name)
    (catch Exception _e
      nil
      ))
  )
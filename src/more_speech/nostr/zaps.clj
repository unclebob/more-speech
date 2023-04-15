(ns more-speech.nostr.zaps
  (:require
    [clj-http.client :as client]
    [clojure.data.json :as json]
    [more-speech.config :as config]
    [more-speech.config :refer [get-db]]
    [more-speech.db.gateway :as gateway]
    [more-speech.logger.default :refer [log-pr]]
    [more-speech.nostr.events :as events]))

(defn get-zap-address-from-tag [event]
  (let [zap-tags (events/get-tag event :zap)
        [zap-addr lud-type :as zap-tag] (first zap-tags)
        lud-type (if (some? lud-type) lud-type "lud16")]
    (cond
      (empty? zap-tags)
      nil

      (not= "lud16" lud-type)
      (throw (Exception. (str lud-type " unimplemented")))

      (not (every? #(= zap-tag %) zap-tags))
      (throw (Exception. "conflicting zaps"))

      :else
      zap-addr)))

(defn get-zap-address-from-profile [event]
  (let [author-id (:pubkey event)
        profile (gateway/get-profile (get-db) author-id)
        zap-addr (:lud16 profile)]
    (cond
      (nil? profile)
      (throw (Exception. "no zap tag or profile"))

      (nil? zap-addr)
      (throw (Exception. "no lud16 in profile"))

      :else
      zap-addr)))

(defn get-zap-address [event]
  (let [zap-address (get-zap-address-from-tag event)
        zap-address (if (some? zap-address)
                      zap-address
                      (get-zap-address-from-profile event))]
    zap-address))

(defn parse-lud16 [lud16]
  (let [match (re-matches config/lud16-pattern lud16)]
    (if (some? match)
      [(nth match 1) (nth match 2)]
      (throw (Exception. (str "bad lud16 format " lud16)))))
  )

(defn lud16->lnurl [lud16]
  (let [[name domain] (parse-lud16 lud16)]
    (str "https://" domain "/.well-known/lnurlp/" name)))

(defn zap-author [event _e]
  (try
    (let [zap-address (get-zap-address event)
          lnurl (lud16->lnurl zap-address)
          ln-response (client/get lnurl)
          response-body (json/read-str (:body ln-response))
          {:strs [callback maxSendable minSendable metadata tag
                  commentAllowed allowsNostr nostrPubkey
                  status reason]} response-body]
      (when (and (some? status) (not= status "OK"))
        (throw (Exception. (str "zap-author Wallet error: " status reason))))
      (prn 'zap-author callback maxSendable minSendable metadata tag commentAllowed allowsNostr nostrPubkey)
      )
    (catch Exception e
      (log-pr 1 'zap-author (.getMessage e))))
  )

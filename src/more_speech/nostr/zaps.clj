(ns more-speech.nostr.zaps
  (:require
    [clj-http.client :as client]
    [clojure.data.json :as json]
    [more-speech.bech32 :as bech32]
    [more-speech.config :as config]
    [more-speech.config :refer [get-db]]
    [more-speech.db.gateway :as gateway]
    [more-speech.logger.default :refer [log-pr]]
    [more-speech.mem :refer :all]
    [more-speech.nostr.event-composers :as composers]
    [more-speech.nostr.events :as events]
    [more-speech.nostr.relays :as relays]
    [more-speech.nostr.util :as util]
    [more-speech.ui.formatter-util :as formatter-util])
  (:use (seesaw [core]))
  (:import (java.net URLEncoder)))

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
  (let [zap-address (get-zap-address-from-tag event)]
    (if (some? zap-address)
      zap-address
      (get-zap-address-from-profile event))))

(defn parse-lud16 [lud16]
  (let [lud16 (.toLowerCase lud16)
        match (re-matches config/lud16-pattern lud16)]
    (if (some? match)
      [(nth match 1) (nth match 2)]
      (throw (Exception. (str "bad lud16 format " lud16)))))
  )

(defn lud16->lnurl [lud16]
  (let [[name domain] (parse-lud16 lud16)]
    (str "https://" domain "/.well-known/lnurlp/" name)))

(defn make-zap-request [wallet-response event amount comment lnurl]
  (let [{:strs [maxSendable minSendable
                commentAllowed allowsNostr]} wallet-response
        _ (when-not allowsNostr
            (throw (Exception. (str "Recipient does not accept Nostr zaps."))))
        _ (when (< amount minSendable)
            (throw (Exception. (str "Amount " amount " is below minimum of " minSendable))))
        _ (when (> amount maxSendable)
            (throw (Exception. (str "Amount " amount " is larger than maximum of " maxSendable))))
        _ (when (and (some? comment)
                     (some? commentAllowed)
                     (> (count comment) commentAllowed))
            (throw (Exception. (str "This wallet restricts comments to " commentAllowed " characters"))))
        recipient (:pubkey event)
        body {:kind 9734
              :content comment
              :tags [(concat ["relays"] (relays/relays-for-reading))
                     ["amount" (str amount)]
                     ["lnurl" (bech32/encode-str "lnurl" lnurl)]
                     ["p" (util/hexify recipient)]
                     ["e" (util/hexify (:id event))]]}
        [_ request] (composers/body->event body)]
    request))

(defn- ask-for-zap []
  (let [zap-dialog
        (dialog :content
                (flow-panel
                  :items ["Amount in sats"
                          (text :id :amount
                                :text "21"
                                :columns 7)
                          "Comment:"
                          (text :id :comment
                                :text "Zap!"
                                :columns 20)])
                :option-type :ok-cancel
                :success-fn (fn [p]
                              [(* 1000 (Integer/parseInt (text (select (to-root p) [:#amount]))))
                               (text (select (to-root p) [:#comment]))])
                :cancel-fn (fn [_p] nil))]
    (show! (pack! zap-dialog))))

(defn zap-author [event _e]
  (try
    (let [zap-address (get-zap-address event)
          lnurl (lud16->lnurl zap-address)
          ln-response (client/get lnurl)
          wallet-response (json/read-str (:body ln-response))
          {:strs [callback status reason]} wallet-response]
      (when (and (some? status) (not= status "OK"))
        (throw (Exception. (str "Wallet error: " status reason))))
      (let [[amount comment :as answer] (ask-for-zap)
            _ (when (nil? answer) (throw (Exception. "cancel")))
            zap-request (make-zap-request wallet-response event amount comment lnurl)
            json-request (events/to-json zap-request)
            encoded-request (URLEncoder/encode ^String json-request "UTF-8")
            invoice-request (str callback
                                 "?amount=" amount
                                 "&nostr=" encoded-request
                                 "&lnurl=" lnurl)
            invoice-response (client/get invoice-request)
            invoice-response-status (:status invoice-response)
            _ (when (not= 200 invoice-response-status)
                (throw (Exception. (str "Invoice request failed:" invoice-response-status))))
            invoice-json (json/read-str (:body invoice-response))
            json-status (get invoice-json "status")
            _ (when (and (some? json-status) (= "ERROR" json-status))
                (throw (Exception. (str "Invoice request error: " (get invoice-json "reason")))))
            invoice (get invoice-json "pr")]
        (update-mem :pending-zaps assoc invoice {:id (:id event)
                                                 :amount amount
                                                 :comment comment})
        (util/copy-to-clipboard invoice)
        (alert (str "Invoice is copied to clipboard.\n"
                    "Paste it into your wallet and Zap!\n\n"
                    (formatter-util/abbreviate invoice 20)
                    (subs invoice (- (count invoice) 5)))))
      )
    (catch Exception e
      (log-pr 1 'zap-author (.getMessage e))
      (when (not= "cancel" (.getMessage e))
        (alert (str "Cannot zap. " (.getMessage e)))))))

(defn process-zap-receipt [event]
  (let [[[receipt-invoice]] (events/get-tag event :bolt11)
        transaction (get-mem [:pending-zaps receipt-invoice])]
    (when (some? transaction)
      (let [{:keys [id amount comment]} transaction
            sats (/ amount 1000)]
        (log-pr 1 'got-zap-receipt (util/hexify id) sats 'sats comment)
        (gateway/add-zap-to-event (get-db)
                                  id {:lnurl receipt-invoice
                                      :created-at (util/get-now)
                                      :amount sats
                                      :comment comment})
        (update-mem :pending-zaps dissoc receipt-invoice)))))

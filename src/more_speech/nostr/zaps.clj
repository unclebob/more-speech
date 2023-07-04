(ns more-speech.nostr.zaps
  (:require
    [clj-http.client :as client]
    [clojure.core.async :as async]
    [clojure.data.json :as json]
    [clojure.string :as string]
    [more-speech.bech32 :as bech32]
    [more-speech.config :as config]
    [more-speech.config :refer [get-db]]
    [more-speech.db.gateway :as gateway]
    [more-speech.logger.default :refer [log-pr]]
    [more-speech.mem :refer :all]
    [more-speech.nostr.elliptic-signature :as es]
    [more-speech.nostr.event-composers :as composers]
    [more-speech.nostr.events :as events]
    [more-speech.nostr.relays :as relays]
    [more-speech.nostr.util :as util]
    [more-speech.relay :as relay]
    [more-speech.ui.formatter-util :as formatter-util]
    [more-speech.ui.formatters :as formatters]
    [more-speech.util.fortune-messages :as fortune]
    [more-speech.websocket-relay :as ws-relay]
    [org.bovinegenius.exploding-fish :as uri]
    )
  (:use (seesaw [core]))
  (:import (ecdhJava SECP256K1)
           (java.net URLEncoder)))

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

(defn- decode-lnurl [lnurl]
  (try
    (bech32/address->str lnurl)
    (catch Exception e
      (log-pr 1 'decode-lnurl (.getMessage e) lnurl))))

(defn get-lnurl-from-tag [zap-descriptor]
  (let [zap-tags (events/get-tag zap-descriptor :zap)
        [zap-addr lud-type] (first zap-tags)]
    (cond
      (empty? zap-tags)
      nil

      (= "lud16" lud-type)
      (lud16->lnurl zap-addr)

      (= "lud06" lud-type)
      (decode-lnurl zap-addr)

      :else
      zap-addr)))

(defn get-lnurl-from-profile [author-id]
  (let [profile (gateway/get-profile (get-db) author-id)
        lud16 (:lud16 profile)
        lud06 (:lud06 profile)]
    (cond
      (some? lud16)
      (lud16->lnurl lud16)

      (some? lud06)
      (decode-lnurl lud06)

      :else
      (throw (Exception. "no zap tag or profile"))
      )))

(defn get-lnurl [zap-descriptor]
  (let [zap-address (get-lnurl-from-tag zap-descriptor)]
    (if (some? zap-address)
      zap-address
      (get-lnurl-from-profile (:pubkey zap-descriptor)))))

(defn make-zap-request-event
  ([wallet-response lnurl zap-descriptor]
   (let [{:keys [zap-amount zap-comment id pubkey]} zap-descriptor]
     (make-zap-request-event wallet-response pubkey id zap-amount zap-comment lnurl)))

  ([wallet-response recipient event-id amount comment lnurl]
   (let [{:strs [maxSendable minSendable
                 commentAllowed allowsNostr]} wallet-response
         _ (when-not allowsNostr
             (throw (Exception. (str "Recipient does not accept Nostr zaps."))))
         _ (when (< amount minSendable)
             (throw (Exception. (str "Amount "
                                     (quot amount 1000)
                                     " is below minimum of "
                                     (quot minSendable 1000)))))
         _ (when (> amount maxSendable)
             (throw (Exception. (str "Amount "
                                     (quot amount 1000)
                                     " is larger than maximum of "
                                     (quot maxSendable 1000)))))
         _ (when (and (some? comment)
                      (some? commentAllowed)
                      (> (count comment) commentAllowed))
             (throw (Exception. (str "This wallet restricts comments to " commentAllowed " characters"))))
         body {:kind 9734
               :content comment
               :tags [(concat ["relays"] (relays/relays-for-reading))
                      ["amount" (str amount)]
                      ["lnurl" (bech32/encode-str "lnurl" lnurl)]
                      ["p" (util/hexify recipient)]
                      ["e" (util/hexify event-id)]
                      ]}
         [_ request] (composers/body->event body)]
     request)))

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

(defn open-wallet [lnurl]
  (let [ln-response (client/get lnurl)
        wallet-response (json/read-str (:body ln-response))
        {:strs [status reason]} wallet-response]
    (when (and (some? status) (not= status "OK"))
      (throw (Exception. (str "Wallet error: " status reason))))
    wallet-response))

(defn request-invoice [wallet-response lnurl zap-descriptor]
  (let [zap-request-event (make-zap-request-event wallet-response lnurl zap-descriptor)
        json-request (events/to-json zap-request-event)
        encoded-request (URLEncoder/encode ^String json-request "UTF-8")
        callback (get wallet-response "callback")
        invoice-request (str callback
                             "?amount=" (:zap-amount zap-descriptor)
                             "&nostr=" encoded-request
                             "&lnurl=" lnurl)]
    (client/get invoice-request)))

(defn validate-invoice [invoice-response]
  (let [invoice-response-status (:status invoice-response)
        _ (when (not= 200 invoice-response-status)
            (throw (Exception. (str "Invoice request failed:" invoice-response-status))))
        invoice-json (json/read-str (:body invoice-response))
        json-status (get invoice-json "status")
        _ (when (and (some? json-status) (= "ERROR" json-status))
            (throw (Exception. (str "Invoice request error: " (get invoice-json "reason")))))]
    (get invoice-json "pr")))

(defn get-zap-invoice [zap-descriptor]
  (try
    (let [lnurl (get-lnurl zap-descriptor)
          wallet-response (open-wallet lnurl)
          invoice-response (request-invoice wallet-response lnurl zap-descriptor)
          invoice (validate-invoice invoice-response)]
      (update-mem :pending-zaps
                  assoc invoice {:id (:id zap-descriptor)
                                 :amount (:zap-amount zap-descriptor)
                                 :comment (:zap-comment zap-descriptor)})
      invoice)
    (catch Exception e
      (when (not= "cancel" (.getMessage e))
        (log-pr 2 'zap-author (.getMessage e))
        (when-not (:auto-zap? zap-descriptor)
          (alert (str "Cannot zap. " (.getMessage e))))))))

(defn zap-by-invoice [zap-descriptor]
  (when-let [invoice (get-zap-invoice zap-descriptor)]
    (util/copy-to-clipboard invoice)
    (alert (str "Invoice is copied to clipboard.\n"
                "Paste it into your wallet and Zap!\n\n"
                (formatter-util/abbreviate invoice 20)
                (subs invoice (- (count invoice) 5))))))

(defn get-wc-info [event-chan relay msg]
  (condp = (first msg)
    "EOSE" (do
             (relay/send relay ["CLOSE" (second msg)]))
    "EVENT" (do
              (let [event (nth msg 2)]
                (async/>!! event-chan (util/translate-event event))))
    "OK" (do
           (when-not (nth msg 2)
             (log-pr 1 'get-wc-info 'zap-request-failed (::ws-relay/url relay) msg)))

    (do
      (log-pr 1 'get-wc 'unexpected (::ws-relay/url relay) msg)))
  )

(defn wc-close [relay]
  (log-pr 2 'wc-close (::ws-relay/url relay)))

(defn make-wc-json-request [invoice]
  (events/to-json {"method" "pay_invoice"
                   "params" {"invoice" invoice}}))

(defn compose-wc-request-event [wc-pubkey-hex secret-hex request]
  (let [kind 23194
        tags [[:p wc-pubkey-hex]]
        content request
        secret-key-bytes (util/hex-string->bytes secret-hex)
        secret-pubkey-bytes (es/get-pub-key secret-key-bytes)
        secret-pubkey-hex (util/bytes->hex-string secret-pubkey-bytes)
        recipient-key (util/hex-string->num wc-pubkey-hex)
        sender-key (util/hex-string->num secret-hex)
        shared-secret (SECP256K1/calculateKeyAgreement sender-key recipient-key)
        encrypted-content (SECP256K1/encrypt shared-secret content)
        body {:kind kind
              :tags tags
              :content encrypted-content}
        event (composers/body->event body
                                     secret-hex
                                     secret-pubkey-hex
                                     )]
    event))

(defn get-wc-request-event [event wc]
  (let [wc-map (uri/query-map wc)
        wc-uri (uri/uri wc)
        wc-pubkey-hex (:host wc-uri)
        secret-hex (get wc-map "secret")
        invoice (get-zap-invoice event)
        request (make-wc-json-request invoice)]
    (compose-wc-request-event wc-pubkey-hex secret-hex request)))

(defn zap-by-wallet-connect
  ([zap-descriptor]
   (zap-by-wallet-connect zap-descriptor (async/timeout 60000)))

  ([zap-descriptor event-chan]
   (let [wc (get-mem [:keys :wallet-connect])
         wc-map (uri/query-map wc)
         wc-uri (uri/uri wc)
         wc-pubkey-hex (:host wc-uri)
         wc-pubkey (util/hex-string->num wc-pubkey-hex)
         relay-url (get wc-map "relay")
         secret-hex (get wc-map "secret")
         secret-bytes (util/hex-string->bytes secret-hex)
         secret-pubkey-bytes (es/get-pub-key secret-bytes)
         secret-pubkey-hex (util/bytes->hex-string secret-pubkey-bytes)
         secret (util/hex-string->num secret-hex)]

     (letfn [(process-wallet-response [response-event]
               (let [content (:content response-event)
                     ptag (ffirst (events/get-tag response-event :p))]
                 (if-not (= ptag secret-pubkey-hex)
                   :continue
                   (let [shared-secret (SECP256K1/calculateKeyAgreement secret wc-pubkey)
                         decrypted-content (SECP256K1/decrypt shared-secret content)
                         response (json/read-str decrypted-content)
                         result-type (get response "result_type")
                         error (get response "error")]
                     (if (= "pay_invoice" result-type)
                       (when-not (nil? error)
                         (let [code (get error "code")
                               message (get error "message")]
                           (log-pr 2 'zap-wallet-connect 'response-error response)
                           (when-not (:auto-zap? zap-descriptor)
                             (alert (str "Zap failed: " code " " message)))))
                       (do
                         (log-pr 2 'zap-wallet-connect 'invalid-result-type response)
                         (when-not (:auto-zap? zap-descriptor)
                           (alert (str "Questionable response from wallet:" relay-url)))))))))

             (send-request-and-handle-response [open-relay]
               (relay/send open-relay ["REQ" "ms-resp" {"kinds" [23195] "authors" [wc-pubkey-hex]}])
               (relay/send open-relay (get-wc-request-event zap-descriptor wc))
               (loop [response-event (async/<!! event-chan)]
                 (cond
                   (= 23195 (:kind response-event))
                   (when (= :continue (process-wallet-response response-event))
                     (recur (async/<!! event-chan)))

                   :else
                   (log-pr 1 'zap-by-wallet-connect 'uknown-event relay-url response-event)))

               (relay/close open-relay))]

       (let [relay (ws-relay/make relay-url {:recv (partial get-wc-info event-chan)
                                             :close wc-close})
             request ["REQ" "ms-info" {"kinds" [13194] "authors" [wc-pubkey-hex]}]
             open-relay (relay/open relay)
             _ (relay/send open-relay request)
             info-event (async/<!! event-chan)]
         (cond
           (nil? info-event)
           (do
             (log-pr 1 'zap-by-wallet-connect 'info-timeout relay-url)
             (relay/close open-relay))

           (not (neg? (string/index-of (:content info-event) "pay_invoice")))
           (send-request-and-handle-response open-relay)

           :else
           (do (log-pr 1 'zap-by-wallet-connect 'unknown-result relay-url info-event)
               (when-not (:auto-zap? zap-descriptor)
                 (alert (str "Zap failed.  No info-event from: " relay-url)))
               (relay/close open-relay))))))))

(defn zap-author [event _e]
  (try
    (let [[amount comment :as answer] (ask-for-zap)
          _ (when (nil? answer) (throw (Exception. "cancel")))
          zap-descriptor (assoc event :zap-amount amount :zap-comment comment)]
      (if (some? (get-mem [:keys :wallet-connect]))
        (zap-by-wallet-connect zap-descriptor)
        (zap-by-invoice zap-descriptor)))
    (catch Exception e
      (when (not= "cancel" (.getMessage e))
        (log-pr 2 'zap-author (.getMessage e))))))

(defn- get-fortune []
  (condp = config/auto-thanks-fortune
    :off ""
    :normal (fortune/get-message)
    :markov (fortune/get-markov-message)
    :insane (fortune/get-markov-message :insane)))

(defn auto-thanks [zapper-id]
  (let [zapper-name (formatters/get-best-name zapper-id)
        prefix (if (= :dm config/auto-thanks) "D @" "@")
        message (str "Thank you!\n" (get-fortune))]
    (composers/compose-and-send-text-event
      nil "Auto Thanks"
      (format "%s%s %s" prefix zapper-name message))))

(defn log-zap [zapper-id zappee-id amount-str time content]
  (let [time-str (formatter-util/format-time time)
        zappee-name (formatters/get-best-name zappee-id)
        zapper-name (formatters/get-best-name zapper-id)

        sats (if (empty? amount-str)
               "???????"
               (format "%7d" (quot (Integer/parseInt amount-str) 1000)))]
    (when (or (= (get-mem :pubkey) zapper-id)
              (= (get-mem :pubkey) zappee-id))
      (spit "private/zap.log"
            (format "%s %s sats %s->%s %s\n" time-str sats zapper-name zappee-name content)
            :append true))))

(defn process-zap-receipt [event]
  (let [[[receipt-invoice]] (events/get-tag event :bolt11)
        transaction (get-mem [:pending-zaps receipt-invoice])
        desc (json/read-str (ffirst (events/get-tag event :description)))
        zapper-id (util/unhexify (get desc "pubkey"))
        zappee-id (util/unhexify (ffirst (events/get-tag event :p)))
        time (:created-at event)
        content (get desc "content")
        desc-tags (get desc "tags")
        amount-tag (first (filter #(= "amount" (first %)) desc-tags))
        amount-str (second amount-tag)]

    (when-not (config/is-test-run?)
      (when (= zappee-id (get-mem :pubkey))
        (when-not (= :off config/auto-thanks)
          (auto-thanks zapper-id)))
      (log-zap zapper-id zappee-id amount-str time content))

    (when (some? transaction)
      (let [{:keys [id amount comment]} transaction
            sats (/ amount 1000)]
        (log-pr 2 'got-zap-receipt (util/hexify id) sats 'sats comment)
        (set-mem :refresh-main-window true)
        (gateway/add-zap-to-event (get-db)
                                  id {:lnurl receipt-invoice
                                      :created-at (util/get-now)
                                      :amount sats
                                      :comment comment})
        (update-mem :pending-zaps dissoc receipt-invoice)))))

(defn auto-zap [amount recipient-id event-id message]
  (log-pr 2 'auto-zap amount recipient-id message)
  (let [zap-descriptor {:zap-amount (* 1000 amount)
                        :pubkey recipient-id
                        :zap-comment message
                        :auto-zap? true
                        :id event-id}]
    (zap-by-wallet-connect zap-descriptor))
  )


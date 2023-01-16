(ns more-speech.websocket-relay-spec
  (:require [speclj.core :refer :all]
            [clojure.core.async :as async]
            [more-speech
             [relay :as relay]
             [websocket-relay :as ws-relay]]))

(describe "websocket-relay"
  (it "can be made"
    (should= {::relay/type ::ws-relay/websocket
              ::ws-relay/url "url"
              ::ws-relay/chan :some-channel
              ::ws-relay/socket nil
              ::ws-relay/open? false}
             (ws-relay/make "url" :some-channel)))

  (it "can open and close"
    (pending "be nice to relay.damus.io")
    (let [relay (ws-relay/make "wss://relay.damus.io" :some-chan)
          relay-open (relay/open relay)
          relay-closed (relay/close relay-open)]
      (should (::ws-relay/open? relay-open))
      (should-not (::ws-relay/open? relay-closed))))

  (it "can send and receive"
    (pending "be nice to relay.damus.io")
    (let [chan (async/chan)
          relay (ws-relay/make "wss://relay.damus.io" chan)
          relay-open (relay/open relay)
          _ (relay/send relay-open ["test"])
          f-reply (future (async/<!! chan))
          reply (deref f-reply 1000 :timeout)
          _ (relay/close relay-open)]
      (should= [["NOTICE" "could not parse command"]
                "wss://relay.damus.io"]
               reply)))
  )


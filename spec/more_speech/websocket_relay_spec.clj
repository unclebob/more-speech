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
              ::ws-relay/callbacks :some-callbacks
              ::ws-relay/socket nil}
             (ws-relay/make "url" :some-callbacks)))

  (it "can open and close"
    (pending "be nice to relay.damus.io")
    (let [relay (ws-relay/make "wss://relay.damus.io" :some-f)
          relay-open (relay/open relay)
          relay-closed (relay/close relay-open)]
      (should (::ws-relay/open? relay-open))
      (should-not (::ws-relay/open? relay-closed))))

  (it "can send and receive"
    (pending "be nice to relay.damus.io")
    (let [chan (async/chan)
          recv-f (fn [relay msg] (async/>!! chan [relay msg]))
          relay (ws-relay/make "wss://relay.damus.io" recv-f)
          relay-open (relay/open relay)
          _ (relay/send relay-open ["test"])
          f-reply (future (async/<!! chan))
          [relay-r reply] (deref f-reply 1000 :timeout)
          _ (relay/close relay-open)]
      (should= relay relay-r )
      (should= ["NOTICE" "could not parse command"] reply)))
  )


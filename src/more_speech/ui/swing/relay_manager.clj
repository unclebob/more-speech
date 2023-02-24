(ns more-speech.ui.swing.relay-manager
  (:use [seesaw core])
  (:require [more-speech.mem :refer :all]
            [more-speech.nostr.protocol :as protocol]
            [more-speech.config :as config]
            [more-speech.nostr.util :as util]))

(defn reconnect-to-relay [url]
  (let [relay (get-in @relays [url :connection])
        relay (if (some? relay) relay (protocol/make-relay url))
        now (util/get-now)]
    (-> relay protocol/close-relay protocol/connect-to-relay)
    (protocol/subscribe-to-relay url config/subscription-id-base (- now 3600) now)
    ))

(defn set-relay-read [label url read-type _e]
  (swap! relays assoc-in [url :read] read-type)
  (config! label :text (str read-type))
  (reconnect-to-relay url))

(defn set-relay-write [label url write-type _e]
  (swap! relays assoc-in [url :write] write-type)
  (config! label :text (str write-type))
  (reconnect-to-relay url))

(defn read-click [url e]
  (when (.isPopupTrigger e)
    (let [x (.x (.getPoint e))
          y (.y (.getPoint e))
          label (.getComponent e)
          p (popup :items [(action :name "none" :handler (partial set-relay-read label url :read-none))
                           (action :name "all" :handler (partial set-relay-read label url :read-all))
                           (action :name "trusted" :handler (partial set-relay-read label url :read-trusted))
                           (action :name "web of trust" :handler (partial set-relay-read label url :read-web-of-trust))])]
      (.show p (to-widget e) x y))))

(defn write-click [url e]
  (when (.isPopupTrigger e)
    (let [x (.x (.getPoint e))
          y (.y (.getPoint e))
          label (.getComponent e)
          p (popup :items [(action :name "true" :handler (partial set-relay-write label url true))
                           (action :name "false" :handler (partial set-relay-write label url false))
                           ])]
      (.show p (to-widget e) x y))))

(defn is-connected? [url]
  (some? (get-in @relays [url :connection])))

(defn make-relay-element [url]
  (let [relay (get @relays url)
        name (label :text (re-find config/relay-pattern url) :size [250 :by 20])
        connection-label (label :text (if (is-connected? url) "✓" "X") :size [10 :by 20])
        read-label (text :text (str (:read relay)) :editable? false :size [100 :by 20])
        write-label (text :text (str (:write relay)) :size [50 :by 20])
        element (horizontal-panel :size [500 :by 20] :items [name connection-label read-label write-label])]
    (listen read-label :mouse-pressed (partial read-click url))
    (listen write-label :mouse-pressed (partial write-click url))
    element))

(defn is-active-url? [url]
  (let [relay-descriptor (get @relays url)
        read-state (:read relay-descriptor)
        write-state (:write relay-descriptor)
        is-reading? (or (= :read-all read-state)
                        (= :read-trusted read-state)
                        (= :read-web-of-trust read-state))
        is-writing? write-state]
    (or is-reading? is-writing?)))

(defn show-relay-manager [_e]
  (let [relay-frame (frame :title "Relays" :size [500 :by 500])
        all-relay-urls (set (keys @relays))
        active-urls (sort (filter is-active-url? all-relay-urls))
        inactive-urls (sort (remove is-active-url? all-relay-urls))
        connected-elements (map make-relay-element active-urls)
        unconnected-elements (map make-relay-element inactive-urls)
        relay-box (scrollable (vertical-panel :items (concat connected-elements unconnected-elements)))
        ]
    (config! relay-frame :content relay-box)
    (show! relay-frame)))


(ns more-speech.ui.swing.relay-panel
  (:use [seesaw core])
  (:require [more-speech.nostr.relays :refer [relays]]))

(defn make-relay-control-panel [relay-url]
  (let [relay (get @relays relay-url)
        read-check-box (checkbox :text "R" :enabled? false :selected? (:read relay) :id :read)
        write-check-box (checkbox :text "W" :enabled? false :selected? (:write relay) :id :write)
        connected-check-box (checkbox :text "Connected" :enabled? false :selected? (some? (:connection relay)) :id :connected)
        subscribed-check-box (checkbox :text "Subscribed" :enabled? false :selected? (:subscribed relay) :id :subscribed)
        relay-label (flow-panel :align :left
                                :items [(label :text relay-url :halign :left)])
        buttons (flow-panel :align :left
                            :items [read-check-box write-check-box connected-check-box subscribed-check-box])
        control-panel (vertical-panel :items [relay-label buttons (separator :border 1)])]
    control-panel))

(defn make-relay-panel []
  (let [urls (keys @relays)
        relay-control-panels (for [url urls] (make-relay-control-panel url))
        panel-map (zipmap urls relay-control-panels)
        relay-panel (vertical-panel :items relay-control-panels :id :relay-panel :user-data panel-map)]
    relay-panel))

(defn update-relay-panel [ui-context]
  (let [frame (:frame @ui-context)
        relay-panel (select frame [:#relay-panel])
        panel-map (config relay-panel :user-data)]
    (loop [urls (keys @relays)]
      (if (empty? urls)
        nil
        (let [url (first urls)
              relay (get @relays url)
              this-relay-panel (get panel-map url)
              connected (select this-relay-panel [:#connected])
              subscribed (select this-relay-panel [:#subscribed])]
          (config! connected :selected? (some? (:connection relay)))
          (config! subscribed :selected? (:subscribed relay))
          (recur (rest urls)))))
    )
  )

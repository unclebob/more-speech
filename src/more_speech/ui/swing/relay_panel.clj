(ns more-speech.ui.swing.relay-panel
  (:use [seesaw core])
  (:require [more-speech.nostr.relays :refer [relays]]
            [more-speech.mem :refer :all]))

(defn make-relay-control-panel [relay-url]
  (let [relay (get @relays relay-url)
        read-check-box (checkbox :text "R" :enabled? false :selected? (:read relay) :id :read)
        write-check-box (checkbox :text "W" :enabled? false :selected? (:write relay) :id :write)
        connected-check-box (checkbox :text "Connected" :enabled? false :selected? (some? (:connection relay)) :id :connected)
        subscribed-check-box (checkbox :text "Subscribed" :enabled? false :selected? (:subscribed relay) :id :subscribed)
        relay-panel (flow-panel :align :left
                                :items [read-check-box write-check-box connected-check-box subscribed-check-box
                                        (label :text relay-url :halign :left)])]
    relay-panel))

(defn make-relay-panel []
  (let [urls (keys @relays)
        relay-control-panels (for [url urls] (make-relay-control-panel url))
        panel-map (zipmap urls relay-control-panels)
        relay-panel (vertical-panel :items relay-control-panels :id :relay-panel :user-data panel-map)]
    relay-panel))


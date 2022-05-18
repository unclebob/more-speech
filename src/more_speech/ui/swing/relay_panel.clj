(ns more-speech.ui.swing.relay-panel
  (:use [seesaw core])
  (:require [more-speech.nostr.relays :refer [relays]]))

(defn make-relay-control-panel [relay]
  (let [read-check-box (checkbox :text "R")
        write-check-box (checkbox :text "W")
        relay-label (flow-panel :align :left
                                :items [(label :text relay :halign :left)])
        buttons (flow-panel :align :left
                            :items [read-check-box write-check-box])
        control-panel (vertical-panel :items [relay-label buttons (separator :border 1)])]
    control-panel))

(defn make-relay-panel []
  (let [relay-control-panels (for [url (keys @relays)] (make-relay-control-panel url))
        relay-panel (vertical-panel :items relay-control-panels)]
    relay-panel))

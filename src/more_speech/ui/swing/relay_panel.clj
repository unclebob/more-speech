(ns more-speech.ui.swing.relay-panel
  (:use [seesaw core])
  (:require [more-speech.nostr.protocol :as protocol]))

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
  (let [relay-control-panels (for [relay protocol/relays] (make-relay-control-panel relay))
        relay-panel (vertical-panel :items relay-control-panels)]
    relay-panel))

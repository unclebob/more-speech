(ns more-speech.ui.swing.ui-context
  (:require [clojure.spec.alpha :as s]))

(def ui-context (atom {:frame nil
                       :event-context nil
                       :node-map {}
                       :orphaned-references {}
                       :selected-tab nil}))

(s/def ::id number?)
(s/def ::orphaned-references (s/map-of ::id (s/coll-of ::id :kind set?)))
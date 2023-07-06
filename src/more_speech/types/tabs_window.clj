(ns more-speech.types.tabs-window
  (:require [clojure.spec.alpha :as s])
  (:import (javax.swing JList)))

(s/def ::id number?)
(s/def ::tab-name string?)
(s/def ::selected (s/coll-of
                    (s/or :id ::id
                          :match string?)))
(s/def ::blocked (s/coll-of ::id))
(s/def ::selected-listbox #(instance? JList %))
(s/def ::tab-data (s/keys :opt-un [::selected
                                   ::selected-listbox
                                   ::blocked]))

(s/def ::all-ids (s/coll-of ::id))

; a map of tab names to tab data or :all-ids to a list of ids.
(s/def ::tabs-window
  (s/or :nil nil?
        :map (s/map-of
               (s/or :tab-data ::tab-name
                     :all-ids #(= :all-ids %))
               (s/or :tab-data ::tab-data
                     :all-ids ::all-ids))))

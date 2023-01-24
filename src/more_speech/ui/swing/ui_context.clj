(ns more-speech.ui.swing.ui-context
  (:require [clojure.spec.alpha :as s]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.in-memory :as in-memory]))

(def ui-context (atom {:frame nil
                       :event-context nil
                       :node-map {}
                       :orphaned-references {}
                       :selected-tab nil}))

(s/def ::id number?)
(s/def ::orphaned-references (s/map-of ::id (s/coll-of ::id :kind set?)))

(defn get-event-state
  ([]
  @(:event-context @ui-context))

  ([field]
   (get @(:event-context @ui-context) field))
  )

(defn get-db []
  {::gateway/type ::in-memory/type
   :data (:event-context @ui-context)})

(defn get-mem []
  (:event-context @ui-context))
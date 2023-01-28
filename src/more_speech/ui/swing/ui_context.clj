(ns more-speech.ui.swing.ui-context
  (:require [clojure.spec.alpha :as s]))

(def ui-context (atom {:frame nil
                       :event-context (atom nil)
                       :node-map {}
                       :orphaned-references {}}))

(s/def ::id number?)
(s/def ::orphaned-references (s/map-of ::id (s/coll-of ::id :kind set?)))

(defn get-mem
  ([]
   (:event-context @ui-context))

  ([field]
   (if (coll? field)
     (get-in @(get-mem) field)
     (get @(get-mem) field)))
  )

(defn set-mem [key value]
  (swap! (get-mem) assoc key value))

(defn clear-mem []
  (reset! (get-mem) {}))
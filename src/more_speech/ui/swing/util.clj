(ns more-speech.ui.swing.util
  (:require [more-speech.ui.swing.ui-context :refer :all])
  (:use [seesaw core]))

(defn clear-popup [popup]
  (while (not (empty? (.getSubElements popup)))
    (.remove popup 0)))

(defn select-tab [tab-id]
  (prn 'select-tab tab-id)
  (let [frame (:frame @ui-context)
        tabbed-panel (select frame [:#header-tab-panel])
        tab-component (select tabbed-panel [(keyword (str "#tab-" (name tab-id)))])]
    (selection! tabbed-panel tab-component)))
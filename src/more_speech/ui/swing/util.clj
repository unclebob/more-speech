(ns more-speech.ui.swing.util
  (:require [more-speech.ui.swing.ui-context :refer :all])
  (:use [seesaw core]))

(defn clear-popup [popup]
  (while (not (empty? (.getSubElements popup)))
    (.remove popup 0)))


(defn get-tab-index [name]
  (loop [tab-list (:tabs-list @(:event-context @ui-context))
         index 0]
    (cond
      (empty? tab-list) nil
      (= name (:name (first tab-list))) index
      :else (recur (rest tab-list) (inc index)))))

(defn select-tab
  "Select tab by name or index"
  [tab-selector]
  (let [frame (:frame @ui-context)
        tabbed-panel (select frame [:#header-tab-panel])
        tab-index (if (number? tab-selector)
                    tab-selector
                    (get-tab-index tab-selector))]
    (selection! tabbed-panel tab-index)))


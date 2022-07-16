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

(defn select-tab [tab-name]
  (let [frame (:frame @ui-context)
        tabbed-panel (select frame [:#header-tab-panel])]
    (selection! tabbed-panel (get-tab-index tab-name))))


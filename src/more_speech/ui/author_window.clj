(ns more-speech.ui.author-window
  (:require
    [clojure.string :as string]
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.text-window :refer [text-window-controls]]
    [more-speech.ui.cursor :as text]
    [more-speech.content.article :as a]
    [more-speech.ui.graphics :as g]))

(declare get-author-height
         draw-authors
         update-authors)

(defrecord author-window-controls []
  text-window-controls
  (get-element-height [_c state]
    (get-author-height state))
  (draw-elements [_c state frame]
    (draw-authors state frame))
  (update-elements [_c state frame]
    (update-authors state frame))
  )

(defn get-author-height [state]
  (let [graphics (get-in state [:application :graphics])
        line-height (g/line-height graphics)]
    line-height))

(defn draw-author [frame cursor author]
  (let [g (:graphics cursor)]
    (g/text-align g [:left])
    (g/text-color g [0 0 0])
    (text/render cursor frame (a/markup-author author))))

(defn draw-authors [state frame]
  (let [application (:application state)
        g (:graphics application)
        display-position (get frame :display-position 0)
        n-elements (get frame :n-elements 60)
        authors (:nicknames application)
        authors (sort-by #(string/lower-case (text/nil->blank (second %))) authors)
        authors (drop display-position authors)
        authors (take n-elements authors)]
    (g/text-align g [:left])
    (g/text-color g [0 0 0])
    (loop [cursor (text/->cursor g 0 (g/line-height g) 5)
           authors authors]
      (if (empty? authors)
        cursor
        (recur (draw-author frame cursor (first authors))
               (rest authors))))))

(defn update-authors [state frame]
  (let [frame-path (:path frame)
        authors (get-in state [:application :nicknames])]
    (assoc-in state (concat frame-path [:total-elements]) (count authors))))
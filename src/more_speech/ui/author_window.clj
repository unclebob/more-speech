(ns more-speech.ui.author-window
  (:require
    [clojure.string :as string]
    [more-speech.nostr.util :refer [num->hex-string]]
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.text-window :refer [text-window-controls]]
    [more-speech.ui.cursor :as text]
    [more-speech.ui.graphics :as g]
    [more-speech.ui.formatters :as f]
    [clojure.spec.alpha :as s]))

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

(s/def ::author-nickname string?)
(s/def ::author-pubkey string?)
(s/def ::author-nickname-tuple (s/tuple ::author-pubkey ::author-nickname))

(defn abbreviate-key [pubkey]
  (f/abbreviate pubkey 8))

(defn markup-author [[pubkey name]]
  [:bold
   (abbreviate-key (num->hex-string pubkey))
   :regular
   " - "
   name
   :new-line
   ])

(defn draw-author [frame cursor author]
  (let [g (:graphics cursor)]
    (g/text-align g [:left])
    (g/text-color g [0 0 0])
    (text/render cursor frame author)))

(defn draw-authors [state frame]
  (let [application (:application state)
        g (:graphics application)
        authors (:displayed-elements frame)]
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
        frame (get-in state frame-path)
        authors (get-in state [:application :nicknames])
        frame (assoc frame :total-elements (count authors))
        display-position (get frame :display-position 0)
        n-elements (min (count authors)  (:n-elements frame))
        sorted-authors (sort-by #(string/lower-case (text/nil->blank (second %))) authors)
        authors-to-display (drop display-position sorted-authors)
        authors-to-display (take n-elements authors-to-display)
        marked-up-authors (map markup-author authors-to-display)
        frame (assoc frame :displayed-elements marked-up-authors)
        state (assoc-in state frame-path frame)]
    state))
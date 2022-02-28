(ns more-speech.ui.article-window-controls
  (:require [more-speech.ui.text-window :refer [text-window-controls
                                                get-element-height
                                                draw-elements
                                                update-elements]]
            [more-speech.ui.graphics :as g]))

(declare get-article-line-height
         draw-articles
         update-articles)

(defrecord article-window-controls []
  text-window-controls
  (get-element-height [_c state]
    (get-article-line-height state))
  (draw-elements [_c state frame]
    (draw-articles state frame))
  (update-elements [_c state frame]
    (update-articles state frame))
  )

(defn get-article-line-height [state]
  (let [graphics (get-in state [:application :graphics])
        line-height (g/line-height graphics)]
    line-height))

(defn draw-articles [_state _frame]
  )

(defn update-articles [_state _frame]
  )
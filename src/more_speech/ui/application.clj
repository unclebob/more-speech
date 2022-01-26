(ns more-speech.ui.application
  (:require [more-speech.ui.cursor :as text]
            [more-speech.ui.widget :refer [widget
                                           draw-widget
                                           draw-child-widgets
                                           setup-child-widgets]]
            [more-speech.ui.article-window :refer [map->article-window
                                                   draw-article-window]]
            [more-speech.ui.author-window :refer [map->author-window
                                                  draw-author-window]]
            [more-speech.ui.graphics :as g]))

(declare setup-application)

(defrecord application [path graphics articles nicknames]
  widget
  (setup-widget [widget state]
    (setup-application widget path state))
  (update-widget [widget state])

  (draw-widget [application state]
    (draw-child-widgets application state)
    )

  (mouse-up [widget state position])
  (mouse-down [widget state position])
  )

(defn- setup-application [application path state]
  (let [graphics (:graphics application)]
    (assoc application
      :articles []
      :nicknames {}
      :article-window (map->article-window
                        {:x 50 :y 10 :w (g/pos-width graphics 105) :h (- (g/screen-height graphics) 100)})

      :author-window (map->author-window
                       {:x (+ 50 (g/pos-width graphics 110)) :y 10
                        :w (g/pos-width graphics 30) :h (- (g/screen-height graphics) 100)})
      )
    ))

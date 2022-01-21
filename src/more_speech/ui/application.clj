(ns more-speech.ui.application
  (:require [more-speech.ui.text :as text]
            [more-speech.ui.widget :refer [widget
                                           draw-widget
                                           draw-child-widgets]]
            [more-speech.ui.article-window :refer [map->article-window
                                                   draw-article-window]]
            [more-speech.ui.author-window :refer [map->author-window
                                                  draw-author-window]]
            [more-speech.ui.graphics :as g]))

(defrecord application [widgets]
  widget
  (setup-widget [widget state])
  (update-widget [widget state])

  (draw-widget [application state]
    (draw-child-widgets application state)
    )

  (mouse-up [widget state position])
  (mouse-down [widget state position])
  )

(defn make-application [bold regular]
  (let [g (g/->quil-graphics)]
    (map->application
      {:graphics g
       :articles []
       :nicknames {}

       :article-window (map->article-window
                         {:x 50 :y 10 :w (text/pos-width 105) :h (- (g/screen-height g) 100)
                          :fonts {:bold bold :regular regular}
                          })

       :author-window (map->author-window
                        {:x (+ 50 (text/pos-width 110)) :y 10
                         :w (text/pos-width 30) :h (- (g/screen-height g) 100)
                         :fonts {:bold bold :regular regular}})
       :widgets [:article-window :author-window]
       }
      )))

(ns more-speech.ui.config)

(def no-fill [nil])
(def black [0 0 0])
(def white [255 255 255])


(def thumb-normal [200 200 200])
(def thumb-dragged [150 150 150])
(def thumb-h 15)
(def thumb-margin 5)

(def scroll-bar-w 20)
(def scroll-bar-button-w 15)
(def scroll-bar-button-h 15)
(def scroll-bar-button-top-margin 5)
(def scroll-bar-button-bottom-margin (+ scroll-bar-button-top-margin
                                        scroll-bar-button-h))

(def article-window-dimensions
  {:x 50 :y 10
   :char-width 105
   :bottom-margin 100})

(def header-frame-dimensions
  {:left-margin 1
   :right-margin 0
   :top-margin 1
   :bottom-margin 1})

(def header-lines 2)
(def header-top-margin 5)
(def header-bottom-margin 5)

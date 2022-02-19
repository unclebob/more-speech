;;
;; Application is the highest level widget and must be the :application
;; member of state.
;;
;; Members
;; :graphics -- The instance of the graphics protocol.
;; :update-articles -- Set to true if the article window content needs updating.
;; :mouse-locked-to -- nil if no lock.  Otherwise the path of the widget to which
;;                     the mouse is locked.
;;

(ns more-speech.ui.application
  (:require [clojure.spec.alpha :as s]
            [more-speech.ui.widget :refer [widget
                                           draw-widget
                                           draw-child-widgets
                                           setup-child-widgets]]
            [more-speech.ui.article-window :refer [map->article-window
                                                   draw-article-window]]
            [more-speech.ui.author-window :refer [map->author-window
                                                  draw-author-window]]
            [more-speech.ui.graphics :as g]
            [more-speech.nostr.events :as events]
            [more-speech.ui.config :as config]))

(s/def ::path (s/tuple [keyword?]))
(s/def ::graphics #(satisfies? g/graphics %))
(s/def ::mouse-locked-to #(or (nil? %) (satisfies? widget %)))
(s/def ::nicknames (s/map-of number? string?))
(s/def ::chronological-text-events (s/coll-of number?))
(s/def ::text-event-map (s/map-of number? ::events/event))
(s/def ::open-thread (s/coll-of number? :kind set?))
(s/def ::update-articles boolean?)
(s/def ::application (s/keys :req-un [::path
                                      ::graphics
                                      ::update-articles
                                      ::mouse-locked-to
                                      ::nicknames
                                      ::chronological-text-events
                                      ::text-event-map
                                      ::open-thread
                                      ]))

(declare setup-application)

(defrecord application [path graphics update-articles mouse-locked-to]
  widget
  (setup-widget [widget state]
    (setup-application widget path state))
  (update-widget [widget state]
    state)
  (draw-widget [application state]
    (draw-child-widgets application state))
  )

(defn- setup-application [application path state]
  (let [graphics (:graphics application)
        bold (get-in graphics [:fonts :bold])]
    (g/text-font graphics bold)
    (assoc application
      :nicknames {}
      :chronological-text-events []
      :text-event-map {}
      :open-thread #{}
      :article-window (map->article-window
                        {:x (:x config/article-window-dimensions)
                         :y (:y config/article-window-dimensions)
                         :w (g/pos-width graphics (:char-width config/article-window-dimensions))
                         :h (- (g/screen-height graphics) (:bottom-margin config/article-window-dimensions))})

      :author-window (map->author-window
                       {:x (+ 50 (g/pos-width graphics 110)) :y 10
                        :w (g/pos-width graphics 30) :h (- (g/screen-height graphics) 100)})
      )
    ))


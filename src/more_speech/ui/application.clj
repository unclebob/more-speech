;;
;; Application is the highest level widget and must be the :application
;; member of state.
;;
;; Members
;; :graphics -- The instance of the graphics protocol.
;; :this-update -- Set of widgets to update on this pass. Cleared at end of pass.
;; :next-update -- Set of widgets to update on next pass.  Moved to :this-update
;;                 at end of pass.
;; :mouse-locked-to -- nil if no lock.  Otherwise the path of the widget to which
;;                     the mouse is locked.
;; :keyboard-focus -- nil of no focus.  Otherwise the path of the widget holding
;;                    the focus.
;;

(ns more-speech.ui.application
  (:require [clojure.spec.alpha :as s]
            [more-speech.ui.widget :refer [widget
                                           draw-widget
                                           draw-child-widgets
                                           setup-child-widgets]]
            [more-speech.ui.text-window :refer [map->text-window]]
            [more-speech.ui.header-window :refer [->header-window-controls]]
            [more-speech.ui.article-window :refer [->article-window-controls]]
            [more-speech.ui.author-window :refer [->author-window-controls]]
            [more-speech.ui.edit-window :refer [->edit-window-controls]]
            [more-speech.ui.graphics :as g]
            [more-speech.nostr.events :as events]
            [more-speech.ui.config :as config]))

(s/def ::path (s/tuple [keyword?]))
(s/def ::graphics #(satisfies? g/graphics %))
(s/def ::mouse-locked-to #(or (nil? %) (s/coll-of keyword?)))
(s/def ::keyboard-focus #(or (nil? %) (s/coll-of keyword?)))
(s/def ::nicknames (s/map-of number? string?))
(s/def ::chronological-text-events (s/coll-of (s/tuple [number? number?]) :kind set?))
(s/def ::text-event-map (s/map-of number? ::events/event))
(s/def ::open-thread (s/coll-of number? :kind set?))
(s/def ::this-update (s/coll-of ::path :kind set?))
(s/def ::next-update (s/coll-of ::path :kind set?))
(s/def ::application (s/keys :req-un [::path
                                      ::graphics
                                      ::this-update
                                      ::next-update
                                      ::mouse-locked-to
                                      ::keyboard-focus
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
  (update-widget [_widget state]
    state)
  (draw-widget [application state]
    (draw-child-widgets application state))
  )

(defn- setup-application [application _path _state]
  (let [graphics (:graphics application)
        bold (get-in graphics [:fonts :bold])
        _ (g/text-font graphics bold)
        screen-height (g/screen-height graphics)
        header-window-height (* screen-height (:height-fraction config/header-window-dimensions))
        header-window-width (g/pos-width graphics (:width config/header-window-dimensions))
        header-window-left (:x config/header-window-dimensions)
        header-window-top (:y config/header-window-dimensions)
        article-window-left header-window-left
        article-window-top (+ header-window-top header-window-height config/article-window-top-margin)
        article-window-width header-window-width
        article-window-height (- screen-height
                                 article-window-top
                                 config/article-window-bottom-margin)
        author-window-top (:top-margin config/author-window-dimensions)
        author-window-left-margin (:left-margin config/author-window-dimensions)
        author-window-width (:width config/author-window-dimensions)
        author-window-height (* screen-height (:height-fraction config/author-window-dimensions))
        author-window-left (+ header-window-left author-window-left-margin header-window-width)
        author-window-width (g/pos-width graphics author-window-width)

        edit-window-top (+ author-window-top author-window-height (:top-margin config/edit-window-dimensions))
        edit-window-left author-window-left
        edit-window-width (g/pos-width graphics (:width config/edit-window-dimensions))
        edit-window-height (- screen-height edit-window-top (:bottom-margin config/edit-window-dimensions))
        ]

    (assoc application
      :this-update #{}
      :next-update #{}
      :mouse-locked-to nil
      :keyboard-focus nil
      :nicknames {}
      :chronological-text-events (events/make-chronological-text-events)
      :text-event-map {}
      :open-thread #{}
      :header-window (map->text-window
                       {:title "Article Headers"
                        :x header-window-left
                        :y header-window-top
                        :w header-window-width
                        :h header-window-height
                        :controls (->header-window-controls)
                        })

      :article-window (map->text-window
                        {:title "Selected Article"
                         :x article-window-left
                         :y article-window-top
                         :w article-window-width
                         :h article-window-height
                         :controls (->article-window-controls)})

      :author-window (map->text-window
                       {:title "Authors"
                        :x author-window-left
                        :y author-window-top
                        :w author-window-width
                        :h author-window-height
                        :controls (->author-window-controls)})

      :edit-window (map->text-window
                     {:title "Compose an article."
                      :x edit-window-left
                      :y edit-window-top
                      :w edit-window-width
                      :h edit-window-height
                      :frame-tags {:cursor :text}
                      :controls (->edit-window-controls)})
      )
    ))

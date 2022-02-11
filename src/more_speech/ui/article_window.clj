(ns more-speech.ui.article-window
  (:require
    [more-speech.ui.cursor :as cursor]
    [more-speech.content.article :as a]
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.button :refer [map->button
                                   up-arrow
                                   down-arrow]]
    [more-speech.ui.graphics :as g]
    [more-speech.nostr.util :refer [num->hex-string]]
    [clojure.set :as set]))

(declare draw-article-frame
         draw-articles
         draw-article-window
         scroll-up
         scroll-down)

(defrecord article-frame [x y w h display-position]
  widget
  (setup-widget [widget state]
    widget)
  (update-widget [widget state]
    state)
  (draw-widget [widget state]
    (draw-article-frame state widget)
    state)
  )

(defn draw-article-frame [state frame]
  (let [{:keys [x y w h]} frame
        application (:application state)
        g (:graphics application)]
    (g/with-translation
      g [x y]
      (fn [g]
        (g/stroke g [0 0 0])
        (g/stroke-weight g 2)
        (g/no-fill g)
        (g/rect g [0 0 w h])
        (draw-articles state frame)))))

(defrecord article-window [x y w h page-up page-down]
  widget
  (setup-widget [widget state]
    (let [frame-path (conj (:path widget) :article-frame)
          scroll-up (partial scroll-up frame-path)
          scroll-down (partial scroll-down frame-path)]
      (assoc widget
        :article-frame (map->article-frame {:x (inc x)
                                            :y (inc y)
                                            :w (- w 30)
                                            :h (dec h)
                                            :display-position 0})
        :page-up (map->button {:x (+ x w -20) :y (+ y 20) :h 20 :w 20
                               :left-down scroll-down
                               :left-held scroll-down
                               :draw up-arrow})
        :page-down (map->button {:x (+ x w -20) :y (+ y h -30) :h 20 :w 20
                                 :left-down scroll-up
                                 :left-held scroll-up
                                 :draw down-arrow})
        )))
  (update-widget [widget state]
    state)
  (draw-widget [widget state]
    (draw-article-window state widget))
  )

(defn- scroll-up [widget-path button state]
  (let [article-window (get-in state widget-path)
        articles (get-in state [:application :chronological-text-events])
        display-position (:display-position article-window)
        display-position (min (count articles) (inc display-position))
        article-window (assoc article-window :display-position display-position)
        state (assoc-in state widget-path article-window)]
    state))

(defn- scroll-down [widget-path button state]
  (let [article-window (get-in state widget-path)
        display-position (:display-position article-window)
        display-position (max 0 (dec display-position))
        article-window (assoc article-window :display-position display-position)
        state (assoc-in state widget-path article-window)]
    state))

(defn thread-events
  "returns articles in threaded order."
  ([events event-map open-events]
   (thread-events events event-map open-events 0))
  ([events event-map open-events indent]
  (loop [events events
         threaded-events []
         processed-events #{}]
    (cond
      (empty? events)
      threaded-events

      (contains? processed-events (first events))
      (recur (rest events) threaded-events processed-events)

      :else
      (let [event-id (first events)
            event (get event-map event-id)
            references (:references event)
            no-references? (empty? references)
            not-open? (nil? (open-events event-id))
            no-thread? (or no-references? (and (zero? indent) not-open?))]
        (if no-thread?
          (recur (rest events)
                 (conj threaded-events (assoc event :indent indent))
                 (conj processed-events event-id))
          (let [thread (thread-events references event-map open-events (inc indent))
                threaded-events (conj threaded-events (assoc event :indent indent))
                threaded-events (vec (concat threaded-events thread))
                processed-events (set/union processed-events (set (map :id thread)))]
            (recur (rest events)
                   threaded-events
                   (conj processed-events event-id)))))
      ))))

  (defn- open-button [cursor]
    (cursor/draw-text cursor "+"))

  (defn- null-button [cursor]
    (cursor/draw-text cursor " "))

  (defn draw-article [window cursor article]
    (let [g (:graphics cursor)]
      (g/text-align g [:left])
      (g/fill g [0 0 0])
      (cursor/render cursor window (a/markup-article article)
                     {:open-button open-button
                      :null-button null-button}))
    )

  (defn draw-articles [state window]
    (let [application (:application state)
          g (:graphics application)
          nicknames (:nicknames application)
          article-map (:text-event-map application)
          events (:chronological-text-events application)
          articles (thread-events events article-map (:open-thread application))
          display-position (:display-position window)
          articles (drop display-position articles)
          articles (take 19 articles)]
      (loop [cursor (cursor/->cursor g 0 (g/line-height g) 5)
             articles articles]
        (if (empty? articles)
          cursor
          (let [text-event (first articles)]
            (if (nil? text-event)
              (recur cursor (rest articles))
              (let [{:keys [pubkey created-at content references indent]} text-event
                    name (get nicknames pubkey (num->hex-string pubkey))
                    ref-count (count references)
                    article (a/make-article name created-at content ref-count indent)]
                (recur (draw-article window cursor article)
                       (rest articles)))))))))

  (defn draw-article-window [state window]
    (let [application (:application state)
          g (:graphics application)]
      (g/with-translation
        g [(:x window) (:y window)]
        (fn [g]
          (g/stroke g [0 0 0])
          (g/stroke-weight g 2)
          (g/fill g [255 255 255])
          (g/rect g [0 0 (:w window) (:h window)])
          )
        )))

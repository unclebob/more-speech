(ns more-speech.ui.header-frame
  (:require
    [more-speech.ui.config :as config]
    [more-speech.ui.cursor :as cursor]
    [more-speech.content.article :as a]
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.button :refer [map->button
                                   up-arrow
                                   down-arrow]]
    [more-speech.ui.graphics :as g]
    [more-speech.nostr.util :refer [num->hex-string]]
    [clojure.set :as set]
    [more-speech.ui.app-util :as app]))

(declare setup-header-frame
         update-header-frame
         draw-header-frame
         mouse-wheel
         thread-events
         draw-headers)

(defrecord header-frame [x y w h display-position]
  widget
  (setup-widget [widget state]
    (setup-header-frame state widget))
  (update-widget [widget state]
    (update-header-frame state widget))
  (draw-widget [widget state]
    (draw-header-frame state widget)
    state)
  )

(defn setup-header-frame [state frame]
  (let [graphics (get-in state [:application :graphics])
        line-height (g/line-height graphics)
        header-height (+ config/header-bottom-margin
                         config/header-top-margin
                         (* config/header-lines line-height))
        headers (quot (:h frame) header-height)
        frame (assoc frame :n-headers headers
                           :header-height header-height
                           :mouse-wheel mouse-wheel)]
    frame))

(defn event->header [text-event nicknames]
  (let [{:keys [id pubkey created-at content references indent]} text-event
        name (get nicknames pubkey (num->hex-string pubkey))
        ref-count (count references)
        header (a/make-header id name created-at content ref-count indent)]
    header
    )
  )

(defn events->headers [events nicknames]
  (map #(event->header % nicknames) events))

(defn clear-widgets [frame]
  (loop [elements (keys frame)
         frame frame]
    (if (empty? elements)
      frame
      (let [key (first elements)
            element (get frame key)]
        (if (and (some? element)
                 (satisfies? widget element))
          (recur (rest elements)
                 (dissoc frame key))
          (recur (rest elements) frame))))))

(defn draw-minus [graphics {:keys [x y h w button-state]}]
  (g/with-translation
    graphics [x y]
    (fn [graphics]
      (g/stroke-weight graphics (if (= button-state :in) 2 1))
      (g/stroke graphics [0 0 0])
      (g/no-fill graphics)
      (g/rect graphics [0 0 w h])
      (g/line graphics [0 (quot h 2) w (quot h 2)])
      )))

(defn draw-plus [graphics {:keys [x y h w button-state]}]
  (g/with-translation
    graphics [x y]
    (fn [graphics]
      (g/stroke-weight graphics (if (= button-state :in) 2 1))
      (g/no-fill graphics)
      (g/stroke graphics [0 0 0])
      (g/rect graphics [0 0 w h])
      (g/line graphics [0 (quot h 2) w (quot h 2)])
      (g/line graphics [(quot w 2) 0 (quot w 2) h])
      )))

(defn toggle-thread [button state]
  (let [id (:id button)
        frame-path (drop-last (:path button))
        open-thread (get-in state [:application :open-thread])
        open-thread (if (contains? open-thread id)
                      (disj open-thread id)
                      (conj open-thread id))
        state (assoc-in state [:application :open-thread] open-thread)]
    (app/update-widget state frame-path)))

(defrecord button-creator [state frame graphics])

(defn make-button-creator [state frame]
  (map->button-creator
    {:state state
     :frame frame
     :graphics (get-in state [:application :graphics])
     }))

(defn- make-thread-button [button-creator id index draw]
  (let [graphics (:graphics button-creator)
        frame (:frame button-creator)
        line-height (g/line-height graphics)
        header-height (+ config/header-top-margin
                         config/header-bottom-margin
                         (* config/header-lines line-height))
        y-pos (+ config/header-top-margin
                 (* index header-height))]
    (map->button {:id id
                  :x (+ 5 (:x frame))
                  :y (+ (:y frame) y-pos)
                  :w 10
                  :h 10
                  :draw draw
                  :path (concat (:path frame) [id])
                  :left-down toggle-thread})))

(defn create-thread-buttons [button-creator headers]
  (let [state (:state button-creator)
        open-thread (get-in state [:application :open-thread])]
    (loop [headers headers
           buttons []
           index 0]
      (if (empty? headers)
        buttons
        (let [header (first headers)
              id (:id header)
              indent (:indent header)
              thread-count (:thread-count header)
              draw-f (if (contains? open-thread id) draw-minus draw-plus)]
          (if (and (> thread-count 0)
                   (zero? indent))
            (recur (rest headers)
                   (conj buttons (make-thread-button button-creator id index draw-f))
                   (inc index))
            (recur (rest headers) buttons (inc index))))))))

(defn add-thread-buttons [frame buttons]
  (loop [frame frame
         buttons buttons]
    (if (empty? buttons)
      frame
      (let [button (first buttons)
            id (:id button)]
        (recur (assoc frame id button) (rest buttons))))))

(defn- update-header-frame [state frame]
  (if (app/update-widget? state frame)
    (let [application (:application state)
          event-map (:text-event-map application)
          events (:chronological-text-events application)
          open-thread (:open-thread application)
          threaded-events (thread-events events event-map open-thread)
          total-events (count threaded-events)
          display-position (:display-position frame)
          end-position (min (count threaded-events) (+ display-position (:n-headers frame)))
          displayed-events (subvec threaded-events display-position end-position)
          nicknames (:nicknames application)
          headers (events->headers displayed-events nicknames)
          bc (make-button-creator state frame)
          frame-path (:path frame)
          frame (get-in state frame-path)
          frame (clear-widgets frame)
          buttons (create-thread-buttons bc headers)
          frame (add-thread-buttons frame buttons)
          marked-up-headers (map a/markup-header headers)
          frame (assoc frame :displayed-headers marked-up-headers
                             :total-headers total-events)
          state (assoc-in state frame-path frame)]
      state)
    state))

(defn draw-header-frame [state frame]
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
        (draw-headers state frame)))))

(defn scroll-frame [frame-path state delta]
  (let [frame (get-in state frame-path)
        articles (get-in state [:application :chronological-text-events])
        display-position (:display-position frame)
        display-position (+ display-position delta)
        display-position (min (count articles) display-position)
        display-position (max 0 display-position)
        frame (assoc frame :display-position display-position)
        state (assoc-in state frame-path frame)
        state (app/update-widget state frame)]
    state))

(defn mouse-wheel [widget state clicks]
  (scroll-frame (:path widget) state clicks))

(defn scroll-up [frame-path button state]
  (scroll-frame frame-path state 1))

(defn scroll-down [frame-path button state]
  (scroll-frame frame-path state -1))

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

(defn draw-header [frame cursor header index]
  (let [g (:graphics cursor)
        header-height (+ config/header-top-margin
                         config/header-bottom-margin
                         (* config/header-lines (g/line-height g)))
        cursor (cursor/set-y cursor (+ config/header-top-margin (* index header-height)))]
    (g/text-align g [:left :top])
    (g/fill g [0 0 0])
    (cursor/render cursor frame header))
  )

(defn draw-headers [state frame]
  (let [application (:application state)
        g (:graphics application)
        headers (:displayed-headers frame)]
    (loop [cursor (cursor/->cursor g 0 (g/line-height g) 20)
           headers headers
           index 0]
      (if (empty? headers)
        cursor
        (let [header (first headers)]
          (if (nil? header)
            (recur cursor (rest headers) index)
            (recur (draw-header frame cursor header index)
                   (rest headers) (inc index))))))))
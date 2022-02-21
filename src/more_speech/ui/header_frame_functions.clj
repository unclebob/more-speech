(ns more-speech.ui.header-frame-functions
  (:require [more-speech.ui.config :as config]
            [more-speech.ui.graphics :as g]
            [more-speech.ui.cursor :as cursor]
            [more-speech.nostr.util :refer [num->hex-string]]
            [more-speech.content.article :as a]
            [more-speech.ui.button :refer [map->button
                                           up-arrow
                                           down-arrow]]
            [more-speech.ui.app-util :as app-util]))

(defn get-element-height [state]
  (let [graphics (get-in state [:application :graphics])
        line-height (g/line-height graphics)
        header-height (+ config/header-bottom-margin
                         config/header-top-margin
                         (* config/header-lines line-height))]
    header-height))

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
                  :left-down app-util/toggle-event-thread})))

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
        headers (:displayed-elements frame)]
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
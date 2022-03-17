(ns more-speech.ui.header-window
  (:require [more-speech.ui.config :as config]
            [clojure.spec.alpha :as s]
            [more-speech.ui.formatters :as f]
            [more-speech.ui.graphics :as g]
            [more-speech.ui.cursor :as cursor]
            [more-speech.nostr.util :refer [num->hex-string]]
            [more-speech.ui.button :refer [map->button
                                           up-arrow
                                           down-arrow]]
            [more-speech.ui.app-util :as app-util]
            [more-speech.ui.text-window :refer [text-window-controls
                                                get-element-height
                                                draw-elements
                                                update-elements
                                                key-pressed]]
            [more-speech.ui.widget :as w]
            [clojure.set :as set]))

(declare get-header-height
         draw-headers
         update-headers
         handle-key
         move-selection)

(defrecord header-window-controls []
  text-window-controls
  (get-element-height [_c state]
    (get-header-height state))
  (draw-elements [_c state frame]
    (draw-headers state frame))
  (update-elements [_c state frame]
    (update-headers state frame))
  (key-pressed [_c state frame key]
    (handle-key state frame key))
  )

(defn handle-key [state frame key]
  (condp = (:key key)
    :up (move-selection state frame -1)
    :down (move-selection state frame 1)
    state))

(defn get-header-height [state]
  (let [graphics (app-util/get-graphics state)
        line-height (g/line-height graphics)
        header-height (+ config/header-bottom-margin
                         config/header-top-margin
                         (* config/header-lines line-height))]
    header-height))

(s/def ::id number?)
(s/def ::group string?)
(s/def ::subject string?)
(s/def ::author string?)
(s/def ::time number?)
(s/def ::body string?)
(s/def ::thread-count number?)
(s/def ::header (s/keys :req-un [::id ::group ::subject ::author ::time ::body ::thread-count]))

(defn make-header [id name time body thread-count indent]
  {:id id
   :group ""
   :author name
   :subject "?"
   :time time
   :body body
   :thread-count thread-count
   :indent indent}
  )

(defn event->header [text-event nicknames]
  (let [{:keys [id pubkey created-at content references indent]} text-event
        name (get nicknames pubkey (num->hex-string pubkey))
        ref-count (count references)
        header (make-header id name created-at content ref-count indent)]
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
     :graphics (app-util/get-graphics state)
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

(defn add-buttons [frame prefix buttons]
  (loop [frame frame
         buttons buttons
         index 1]
    (if (empty? buttons)
      frame
      (let [button (first buttons)
            tag (str prefix index)
            button (assoc button :path (concat (:path frame) [tag]))]
        (recur (assoc frame tag button) (rest buttons) (inc index))))))

(defn add-thread-buttons [frame buttons]
  (add-buttons frame "T" buttons))

(defn draw-selector [state graphics {:keys [x y h w button-state id]}]
  (let [selected-header (get-in state [:application :selected-header])
        fill-color
        (cond (= selected-header id) [0 0 0 30]
              (= button-state :in) [0 0 0 10]
              :else :no-fill)]
    (when (not= fill-color :no-fill)
      (g/fill graphics fill-color)
      (g/no-stroke graphics)
      (g/rect graphics [x y w h]))
    )
  )

(defn- select-header [button state]
  (let [state (app-util/select-header (:id button) state)]
    (w/pass-to-parent state button :left-down)
    ))

(defn- make-selection-button [button-creator id index]
  (let [graphics (:graphics button-creator)
        state (:state button-creator)
        frame (:frame button-creator)
        line-height (g/line-height graphics)
        header-height (+ config/header-top-margin
                         config/header-bottom-margin
                         (* config/header-lines line-height))
        y-pos (+ config/header-top-margin
                 (* index header-height))]
    (map->button {:id id
                  :x (+ 20 (:x frame))
                  :y (+ (:y frame) y-pos)
                  :w (- (:w frame) 20)
                  :h (* config/header-lines line-height)
                  :draw (partial draw-selector state)
                  :left-down select-header})))

(defn create-selection-buttons [button-creator headers]
  (loop [headers headers
         buttons []
         index 0]
    (if (empty? headers)
      buttons
      (let [header (first headers)
            id (:id header)]
        (recur (rest headers)
               (conj buttons (make-selection-button button-creator id index))
               (inc index))))))

(defn add-selection-buttons [frame buttons]
  (add-buttons frame "S" buttons))

(defn abbreviate-body [body]
  (f/abbreviate
    body
    (:text-width config/header-window-dimensions)))

(defn abbreviate-author [author]
  (f/abbreviate author 20))

(defn markup-header [header]
  (let [thread-count (:thread-count header)
        indent (get header :indent 0)]
    [
     :regular
     (apply str (repeat indent "•"))
     :bold
     (abbreviate-author (:author header))
     :regular
     (if (pos? thread-count)
       (str " (" thread-count ")")
       "")
     :bold
     :pos 30
     (:subject header)
     :regular
     :pos 60
     (f/format-time (:time header))
     :new-line
     (abbreviate-body (:body header))
     :new-line
     ]))

(defn thread-events
  "returns events in threaded order."
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
                 events-in-thread (set (map :id thread))
                 processed-events (set/union processed-events events-in-thread)]
             (recur (rest events)
                    threaded-events
                    (conj processed-events event-id)))))))))


(defn get-threaded-events [application]
  (let [event-map (:text-event-map application)
        events (:chronological-text-events application)
        open-thread (:open-thread application)
        threaded-events (thread-events events event-map open-thread)]
    threaded-events
    ))

(defn update-headers [state frame]
  (let [application (:application state)
        threaded-events (get-threaded-events application)
        total-events (count threaded-events)
        display-position (:display-position frame)
        end-position (min (count threaded-events) (+ display-position (:n-elements frame)))
        displayed-events (subvec threaded-events display-position end-position)
        nicknames (:nicknames application)
        headers (events->headers displayed-events nicknames)
        bc (make-button-creator state frame)
        frame-path (:path frame)
        frame (get-in state frame-path)
        frame (w/clear-widgets frame)
        thread-buttons (create-thread-buttons bc headers)
        frame (add-thread-buttons frame thread-buttons)
        selection-buttons (create-selection-buttons bc headers)
        frame (add-selection-buttons frame selection-buttons)
        marked-up-headers (map markup-header headers)
        frame (assoc frame :displayed-elements marked-up-headers
                           :total-elements total-events)
        state (assoc-in state frame-path frame)]
    state))

(defn index-of-selection [events id]
  (let [indices (keep-indexed #(if (= id (:id %2)) %1 nil) events)]
    (first indices))
  )

(defn id-of-selection-moved-by [events selected-id delta]
  (let [index (index-of-selection events selected-id)
        index (+ index delta)
        event (get events index)]
    (:id event))
  )

(defn move-selection [state frame delta]
  (let [application (:application state)
        selection (:selected-header application)]
    (if (nil? selection)
      state
      (let [display-position (:display-position frame)
            events (get-threaded-events application)
            new-id (id-of-selection-moved-by events selection delta)]
        (if (or (nil? selection)
                (nil? new-id))
          state
          (let [
                state (app-util/select-header new-id state)
                state (w/redraw-widget state [:application :header-window])
                frame-path (:path frame)
                total-elements (count events)
                display-position (+ delta display-position)
                display-position (min (max 0 display-position) total-elements)
                frame (assoc frame :display-position display-position)
                state (assoc-in state frame-path frame)]
            state))))))

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
    (loop [cursor (cursor/->cursor
                    g
                    0
                    (g/line-height g)
                    (:left-margin config/header-window-dimensions))
           headers headers
           index 0]
      (if (empty? headers)
        cursor
        (let [header (first headers)]
          (if (nil? header)
            (recur cursor (rest headers) index)
            (recur (draw-header frame cursor header index)
                   (rest headers) (inc index))))))))



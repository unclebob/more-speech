;;
;; A window for holding scrollable text.  Paints a scrollbar with thumb and
;; arrows, and responds to mouse-wheel motion.  Derives from widget.
;; Builds and internal text-frame to hold the text.
;;
;; :controls must be a derivative of the text-window-controls protocol.
;;
;; The window is measured by the height of text "elements".  Typically, these
;; are one line high; but could be multiple lines. They are measured in pixels.
;; Scrolling is by elements, not by pixels.  The :display-position field of
;; the text-frame holds the ordinal position of the element the top of
;; the screen.  The :total-elements field holds the total number of elements
;; being scrolled.
;;
;; :display-position is set for you by the scrollbar/mouse-wheel manipulation.
;; :total-elements must be set by the update-elements method of the
;; text-window-controls protocol.  The update-elements method must use the
;; :display-position field to determine which elements should be displayed,
;; and then to communicate those elements to the draw-elements method of the
;; text-window-control protocol, probably using some field in the text-frame
;; or text-window such as :displayed-elements.
;;
;; :frame-tags on the window will be merged with the text-frame.
;;
(ns more-speech.ui.text-window
  (:require
    [more-speech.ui.widget :refer [widget]]
    [more-speech.ui.button :refer [map->button
                                   up-arrow
                                   down-arrow
                                   draw-thumb]]
    [more-speech.ui.graphics :as g]
    [more-speech.nostr.util :refer [num->hex-string]]
    [more-speech.ui.config :as config]
    [more-speech.ui.widget :as w]
    [more-speech.ui.app-util :as app-util]))

(declare setup-text-window
         update-text-window
         draw-text-window
         scroll-up
         scroll-down
         thumb-position
         lock-thumb
         unlock-thumb
         drag-thumb
         setup-text-frame
         update-text-frame
         draw-text-frame
         mouse-wheel
         scroll-frame
         set-focus
         key-pressed-in-frame)

(defrecord text-window [x y w h controls]
  widget
  (setup-widget [widget state]
    (setup-text-window widget state))
  (update-widget [widget state]
    (update-text-window widget state))
  (draw-widget [widget state]
    (draw-text-window state widget)))

(defprotocol text-window-controls
  (get-element-height [controls state]
    "returns the fixed height, in pixels, of the scrolled element.")
  (draw-elements [controls state frame]
    "draws the text elements.")
  (update-elements [controls state frame]
    "Called only if the widget is in [:application :this-update]
    sets :total-elements.")
  (key-pressed [controls state frame key]
    "Called when a key is pressed and this window has the focus.")
  )

(defrecord text-frame [x y w h display-position controls]
  widget
  (setup-widget [widget state]
    (setup-text-frame state widget))
  (update-widget [widget state]
    (update-text-frame state widget))
  (draw-widget [widget state]
    (draw-text-frame state widget)
    state)
  )


(defn setup-text-window [window _state]
  (let [{:keys [x y w h controls]} window
        frame-path (concat (:path window) [:text-frame])
        scroll-up (partial scroll-up frame-path)
        scroll-down (partial scroll-down frame-path)
        frame-tags (:frame-tags window)
        frame (map->text-frame {:x x
                                :y (+ y config/window-title-height)
                                :w (- w config/scroll-bar-w)
                                :h (- h config/window-title-height)
                                :controls controls
                                :display-position 0
                                :total-elements 0})
        frame (merge frame frame-tags)
        sb-button-offset (+ (/ config/scroll-bar-w 2)
                            (/ config/scroll-bar-button-w 2))
        sb-button-x (+ x w (- sb-button-offset) 0.5)
        window (assoc window
                 :left-down set-focus
                 :key-pressed key-pressed-in-frame
                 :text-frame frame
                 :scroll-down (map->button {:x sb-button-x
                                            :y (+ y config/scroll-bar-button-top-margin)
                                            :h config/scroll-bar-button-h
                                            :w config/scroll-bar-button-w
                                            :left-down scroll-down
                                            :left-held scroll-down
                                            :draw up-arrow})
                 :scroll-up (map->button {:x sb-button-x
                                          :y (+ y h (- config/scroll-bar-button-bottom-margin))
                                          :h config/scroll-bar-button-h
                                          :w config/scroll-bar-button-w
                                          :left-down scroll-up
                                          :left-held scroll-up
                                          :draw down-arrow})
                 :thumb (map->button {:x sb-button-x
                                      :y (thumb-position frame)
                                      :h config/thumb-h
                                      :w config/scroll-bar-button-w
                                      :draw draw-thumb
                                      :left-held drag-thumb
                                      :left-down lock-thumb
                                      :left-up unlock-thumb
                                      }))]
    window))

(defn set-focus [widget state]
  (w/set-keyboard-focus state widget))

(defn update-text-window [widget state]
  (let [text-frame (:text-frame widget)
        thumb-pos (thumb-position text-frame)
        thumb-path (concat (:path widget) [:thumb])]
    (assoc-in state (concat thumb-path [:y]) thumb-pos)))

(defn draw-text-window [state {:keys [w] :as window}]
  (let [application (:application state)
        g (:graphics application)
        weight (if (= (:keyboard-focus application) (:path window)) 4 2)
        title-font (get-in g [:fonts :regular])
        _ (g/text-font g title-font)
        title (:title window)
        title-width (g/text-width g title)
        title-pos (- (/ (:w window) 2) (/ title-width 2))
        title-third-height (/ config/window-title-height 3)
        title-half-height (/ config/window-title-height 2)]
    (g/with-translation
      g [(:x window) (:y window)]
      (fn [g]
        (g/stroke g [0 0 0])
        (g/stroke-weight g weight)
        (g/fill g config/white)
        (g/rect-mode g :corner)
        (g/rect g [0 0 (:w window) (:h window)])
        (g/stroke g config/black)
        (g/stroke-weight g 1)
        (g/line g [5 title-third-height
                   (- title-pos 5) title-third-height])
        (g/line g [(+ title-pos title-width 5) title-third-height
                   (- w config/scroll-bar-w) title-third-height])
        (g/line g [5 (* 2 title-third-height)
                   (- title-pos 5) (* 2 title-third-height)])
        (g/line g [(+ title-pos title-width 5) (* 2 title-third-height)
                   (- w config/scroll-bar-w) (* 2 title-third-height)])

        (g/fill g config/black)
        (g/text-align g [:left :center])
        (g/text-font g (get-in g [:fonts :regular]))
        (g/text g [title title-pos title-half-height])
        ))))

(defn scroll-up [frame-path _button state]
  (let [frame (get-in state frame-path)]
    ((:scroll-frame frame) frame-path state 1)))

(defn scroll-down [frame-path _button state]
  (let [frame (get-in state frame-path)]
    ((:scroll-frame frame) frame-path state -1)))

(defn- thumb-drag-height [frame]
  (- (:h frame) (- config/window-title-height)
     (* 2 (+ config/scroll-bar-button-top-margin
             config/scroll-bar-button-h
             config/thumb-margin))
     config/thumb-h))

(defn- thumb-origin [frame]
  (+ (:y frame) (- config/window-title-height)
     config/scroll-bar-button-top-margin
     config/scroll-bar-button-h
     config/thumb-margin))

(defn- thumb-position [text-frame]
  (let [display-position (get text-frame :display-position 0)
        total-elements (get text-frame :total-elements 0)
        height (thumb-drag-height text-frame)]
    (if (zero? total-elements)
      (thumb-origin text-frame)
      (+ (thumb-origin text-frame)
         (* height (/ display-position total-elements))))))

(defn- drag-thumb [button state]
  (let [graphics (app-util/get-graphics state)
        thumb-path (:path button)
        parent-path (drop-last thumb-path)
        text-window-path (drop-last thumb-path)
        text-frame-path (concat text-window-path [:text-frame])
        text-frame (get-in state text-frame-path)
        total-elements (get text-frame :total-elements 0)
        height (thumb-drag-height text-frame)
        top (thumb-origin text-frame)
        [_ my _] (g/get-mouse graphics)
        dy (- my top)
        dy (max dy 0)
        dy (min dy height)
        display-position (* (/ dy height) total-elements)
        state (assoc-in state
                        (concat text-frame-path [:display-position])
                        display-position)]
    (w/redraw-widget state parent-path)))

(defn- lock-thumb [widget state]
  (w/lock-mouse state widget))

(defn- unlock-thumb [_widget state]
  (w/unlock-mouse state))

(defn setup-text-frame [state frame]
  (let [controls (:controls frame)
        element-height (get-element-height controls state)
        elements (quot (:h frame) element-height)
        frame (assoc frame :n-elements elements
                           :mouse-wheel mouse-wheel
                           :scroll-frame scroll-frame
                           )]
    frame))

(defn key-pressed-in-frame [window state key]
  (let [frame (:text-frame window)
        controls (:controls frame)
        state (key-pressed controls state frame key)]
    state))

(defn- update-text-frame [state frame]
  (if (w/redraw-widget? state frame)
    (update-elements (:controls frame) state frame)
    state))

(defn draw-text-frame [state frame]
  (let [{:keys [x y w h controls]} frame
        application (:application state)
        g (:graphics application)]
    (g/with-translation
      g [x y]
      (fn [g]
        (g/stroke g [0 0 0])
        (g/stroke-weight g 2)
        (g/no-fill g)
        (g/rect g [0 0 w h])
        (draw-elements controls state frame)))))

(defn scroll-frame [frame-path state delta]
  (let [frame (get-in state frame-path)
        display-position (:display-position frame)
        total-elements (get frame :total-elements 0)
        display-position (+ display-position delta)
        display-position (min total-elements display-position)
        display-position (max 0 display-position)
        frame (assoc frame :display-position display-position)
        state (assoc-in state frame-path frame)
        state (w/redraw-widget state frame)
        ]
    state))

(defn mouse-wheel [frame state clicks]
  (scroll-frame (:path frame) state clicks))


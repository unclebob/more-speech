;; Widgets form a hierarchy. Any member of a widget that satisfies? the widget
;; protocol will be considered a child widget.  The setup-widget, update-widget,
;; and draw-widget functions are propagated through all the children.
;;
;; Widgets are addressed through their path from the state. The :path member
;; contains that path.  (get-in state (:path widget)) will return the current
;; version of the widget.  (assoc-in state (:path widget) widget) puts any
;; updates to the widget back into the state.
;;
;; The :x and :y members are in global (screen) coordinates.  The :h and :w
;; members are in pixels.  By convention child widgets should fit within the
;; x,y,w,h bounds of their parents.
;;
;; Mouse gestures are routed to widgets by two means.
;;  1. If the mouse-lock-path is not nil, then the mouse gesture is routed
;;     immediately to the widget described by that path.
;;  2. Otherwise the position of the mouse is used to find the deepest
;;     widget that contains that position.  The mouse gesture is then
;;     routed to that widget, or the nearest anscestor widget that contains
;;     the coresponding responder function.
;;
;; Mouse responder functions:
;;  :mouse-wheel [widget state delta] delta is signed number of clicks.
;;  :left-down   [widget state]
;;  :left-up     [widget state]
;;  :left-held   [widget state]

;; Keyboard gestures are routed to the widget described by keyboard-focus-path.
;;
;; Keyboard responder functions:
;;  :key-pressed [widget state {:keys [key key-code raw-key modifiers]}]
;;

(ns more-speech.ui.widget
  (:require [more-speech.util.geometry :refer [inside-rect]]
            [more-speech.ui.graphics :as g]))

(defprotocol widget
  (setup-widget [widget state] "returns the setup widget")
  (update-widget [widget state] "returns state updated.")
  (draw-widget [widget state] "returns nothing.  No state change.")
  )

(def next-update-path [:application :next-update])
(def this-update-path [:application :this-update])
(def mouse-lock-path [:application :mouse-locked-to])
(def keyboard-focus-path [:application :keyboard-focus])

(defn redraw-widget [state maybe-widget]
  (cond
    (satisfies? widget maybe-widget)
    (update-in state next-update-path conj (:path maybe-widget))

    (coll? maybe-widget)
    (update-in state next-update-path conj maybe-widget)

    :else
    (assert false "Bad widget in redraw-widget")
    ))

(defn is-ancestor? [ancestor child]
  (let [ancestor-count (count ancestor)]
    (cond
      (zero? ancestor-count)
      false

      (= ancestor child)
      true

      (> ancestor-count (count child))
      false

      (= (take ancestor-count child) ancestor)
      true

      :else false
      )))

(defn redraw-widget? [state widget]
  (let [path (:path widget)]
    (loop [update-set (get-in state this-update-path [])]
      (if (empty? update-set)
        false
        (if (is-ancestor? (first update-set) path)
          true
          (recur (rest update-set)))))))

(defn set-keyboard-focus [state widget]
  (assoc-in state keyboard-focus-path (:path widget)))

(defn clear-keyboard-focus [state]
  (assoc-in state keyboard-focus-path nil))

(defn get-keyboard-focus [state]
  (get-in state keyboard-focus-path))

(defn lock-mouse [state widget]
  (assoc-in state mouse-lock-path (:path widget)))

(defn unlock-mouse [state]
  (assoc-in state mouse-lock-path nil))

(defn get-mouse-lock [state]
  "returns path of locked widget or nil."
  (get-in state mouse-lock-path nil))

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


(defn get-child-widgets [parent]
  "returns the tags of all children that satisfy the widget protocol."
  (loop [tags (keys parent)
         children []]
    (if (empty? tags)
      children
      (let [child-tag (first tags)
            child (get parent child-tag)]
        (if (satisfies? widget child)
          (recur (rest tags) (conj children child-tag))
          (recur (rest tags) children))))))

(defn do-children [{:keys [path] :as parent} state f]
  (loop [state state
         child-tags (get-child-widgets parent)]
    (if (empty? child-tags)
      nil
      (let [child-tag (first child-tags)
            child-path (concat path [child-tag])
            child (get-in state child-path)
            _ (f child state)
            _ (do-children child state f)]
        (recur state (rest child-tags))))))

(defn update-children [{:keys [path] :as parent} state f]
  (loop [state state
         child-tags (get-child-widgets parent)]
    (if (empty? child-tags)
      state
      (let [child-tag (first child-tags)
            child-path (concat path [child-tag])
            child (get-in state child-path)
            state (f child state)
            child (get-in state child-path)
            state (update-children child state f)]
        (recur state (rest child-tags))))))

(defn draw-child-widgets [parent state]
  (do-children parent state draw-widget))

(defn update-child-widgets [parent state]
  (update-children parent state update-widget))

(defn setup-child-widgets [{:keys [path] :as parent} state]
  (loop [state state
         child-tags (get-child-widgets parent)]
    (if (empty? child-tags)
      state
      (let [child-tag (first child-tags)
            child-path (vec (concat path [child-tag]))
            child (get-in state child-path)
            child (assoc child :path child-path)
            child (setup-widget child state)
            state (assoc-in state child-path child)
            state (setup-child-widgets child state)]
        (recur state (rest child-tags))))
    )
  )

(defn find-deepest-mouse-target [parent mx my]
  (loop [widgets (get-child-widgets parent)]
    (if (empty? widgets)
      nil
      (let [{:keys [x y w h] :as child} (get parent (first widgets))]
        (if (inside-rect [x y w h] [mx my])
          (let [grandchild (find-deepest-mouse-target child mx my)]
            (if (some? grandchild)
              grandchild
              child))
          (recur (rest widgets)))))))

(defn find-deepest-responder [state function]
  (let [application (:application state)
        graphics (:graphics application)
        [x y _] (g/get-mouse graphics)
        widget (find-deepest-mouse-target application x y)]
    (loop [path (:path widget)]
      (let [widget (get-in state path)]
        (if (empty? path)
          nil
          (let [f (get widget function)]
            (if (fn? f)
              widget
              (recur (drop-last path)))))))))

(defn find-mouse-responder [state function]
  "returns widget that should respond to the specified function."
  (let [lock-path (get-mouse-lock state)
        locked-widget (if (some? lock-path) (get-in state lock-path) nil)]
    (if (some? locked-widget)
      (if (fn? (function locked-widget))
        locked-widget
        nil)
      (find-deepest-responder state function))))

(defn pass-to-parent [state widget event & args]
  (loop [path (drop-last (:path widget))]
    (if (= [:application] path)
      state
      (let [widget (get-in state path)
            event-f (get widget event)]
        (if (fn? event-f)
          (apply event-f
                 (concat
                   [widget state]
                   args))
          (recur (drop-last path)))))))

(defn set-cursor [application]
  (let [graphics (:graphics application)
        [mx my _] (g/get-mouse graphics)
        widget (find-deepest-mouse-target application mx my)
        cursor (:cursor widget)
        cursor (if (nil? cursor) :arrow cursor)]
    (g/cursor graphics cursor)))

(defn mouse-wheel [state delta]
  (let [responder (find-mouse-responder state :mouse-wheel)]
    (if (nil? responder)
      state
      ((:mouse-wheel responder) responder state delta))))

(defn mouse-pressed [state _ #_{:keys [x y button]}]
  (let [responder (find-mouse-responder state :left-down)]
    (if (nil? responder)
      state
      ((:left-down responder) responder state))))

(defn mouse-released [state _ #_{:keys [x y]}]
  (let [responder (find-mouse-responder state :left-up)]
    (if (nil? responder)
      state
      ((:left-up responder) responder state))))

(defn mouse-moved [state _ #_{:keys [x y p-x p-y]}] state)

(defn mouse-dragged [state _ #_{:keys [x y p-x p-y button]}]
  (let [responder (find-mouse-responder state :left-held)]
    (if (nil? responder)
      state
      ((:left-held responder) responder state))))

(defn key-pressed [state key #_{:keys [key key-code raw-key modifiers]}]
  (let [responder-path (get-in state keyboard-focus-path)
        responder (get-in state responder-path)
        event-f (get responder :key-pressed)]
    (if (fn? event-f)
      (event-f responder state key)
      state)))
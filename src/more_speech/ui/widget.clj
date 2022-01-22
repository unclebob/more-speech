(ns more-speech.ui.widget)

(defprotocol widget
  (setup-widget [widget state])
  (update-widget [widget state])
  (draw-widget [widget state])
  (mouse-up [widget state position])
  (mouse-down [widget state position]))

(defn do-children [parent state f]
  (loop [widgets (keys (:widgets parent))]
      (if (empty? widgets)
        state
        (do (f (get (:widgets parent) (first widgets)) state)
            (recur (rest widgets)))))
  )

(defn draw-child-widgets [parent state]
  (do-children parent state draw-widget))

(defn setup-child-widgets [parent state]
  (do-children parent state setup-widget)
  )

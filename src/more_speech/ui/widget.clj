(ns more-speech.ui.widget)

(defprotocol widget
  (setup-widget [widget state])
  (update-widget [widget state])
  (draw-widget [widget state])
  (mouse-up [widget state position])
  (mouse-down [widget state position]))

(defn draw-child-widgets [parent state]
  (loop [widgets (keys (:widgets parent))]
    (if (empty? widgets)
      state
      (do (draw-widget (get (:widgets parent) (first widgets)) state)
          (recur (rest widgets))))))

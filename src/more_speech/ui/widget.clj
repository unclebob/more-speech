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

(defn setup-child-widgets [{:keys [path widgets]} state]
  (loop [state state
         child-tags (keys widgets)]
    (if (empty? child-tags)
      state
      (let [parent (get-in state path)
            child-widgets (get parent :widgets)
            child-tag (first child-tags)
            child (get child-widgets child-tag)
            child-path (concat path [:widgets child-tag])
            child (assoc child :path child-path)
            parent (assoc-in parent [:widgets child-tag] child)
            state (assoc-in state path parent)
            state (setup-widget child state)]
        (recur state (rest child-tags))))
    )
  )

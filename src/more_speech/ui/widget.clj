(ns more-speech.ui.widget)

(defprotocol widget
  (setup-widget [widget state])
  (update-widget [widget state])
  (draw-widget [widget state])
  (mouse-up [widget state position])
  (mouse-down [widget state position]))


(defn get-child-widgets [parent]
  (loop [tags (keys parent)
         children []]
    (if (empty? tags)
      children
      (let [child-tag (first tags)
            child (get parent child-tag)]
        (if (satisfies? widget child)
          (recur (rest tags) (conj children child-tag))
          (recur (rest tags) children))))))

(defn do-children [parent state f]
  (loop [state state
         child-tags (get-child-widgets parent)]
      (if (empty? child-tags)
        state
        (recur (f (get parent (first child-tags)) state)
               (rest child-tags)))))


(defn draw-child-widgets [parent state]
  (do-children parent state draw-widget))


(defn setup-child-widgets [{:keys [path] :as parent} state]
  (loop [state state
         child-tags (get-child-widgets parent)]
    (if (empty? child-tags)
      state
      (let [parent (get-in state path)
            child-tag (first child-tags)
            child (get parent child-tag)
            child-path (conj path child-tag)
            child (assoc child :path child-path)
            parent (assoc parent child-tag child)
            state (assoc-in state path parent)
            state (setup-widget child state)
            state (setup-child-widgets child state)]
        (recur state (rest child-tags))))
    )
  )

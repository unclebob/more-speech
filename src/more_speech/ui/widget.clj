(ns more-speech.ui.widget
  (:require [more-speech.ui.graphics :as g]))

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

(defn do-children [{:keys [path] :as parent} state f]
  (loop [state state
         child-tags (get-child-widgets parent)]
    (if (empty? child-tags)
      state
      (let [child-tag (first child-tags)
            child-path (conj path child-tag)
            child (get-in state child-path)
            child (f child state)
            state (assoc-in state child-path child)
            child (get-in state child-path)
            state (do-children child state f)]
        (recur state (rest child-tags))))))

(defn draw-child-widgets [{:keys [path] :as parent} state]
  (loop [state state
           child-tags (get-child-widgets parent)]
      (if (empty? child-tags)
        state
        (let [child-tag (first child-tags)
              child-path (conj path child-tag)
              child (get-in state child-path)
              g (get-in state [:application :graphics])
              _ (g/push-graphics g)
              _ (g/translate g [(:x child) (:y child)])
              _ (g/clip g [0 0 (inc (:w child)) (inc (:h child))])
              child (draw-widget child state)
              _ (g/no-clip g)
              state (assoc-in state child-path child)
              child (get-in state child-path)
              state (draw-child-widgets child state)
              _ (g/pop-graphics g)]
          (recur state (rest child-tags))))))

(defn setup-child-widgets [{:keys [path] :as parent} state]
  (loop [state state
         child-tags (get-child-widgets parent)]
    (if (empty? child-tags)
      state
      (let [child-tag (first child-tags)
            child-path (conj path child-tag)
            child (get-in state child-path)
            child (assoc child :path child-path)
            child (setup-widget child state)
            state (assoc-in state child-path child)
            state (setup-child-widgets child state)]
        (recur state (rest child-tags))))
    )
  )

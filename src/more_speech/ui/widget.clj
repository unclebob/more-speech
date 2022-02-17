(ns more-speech.ui.widget
  (:require [more-speech.util.geometry :refer [inside-rect]]))

(defprotocol widget
  (setup-widget [widget state] "returns the setup widget")
  (update-widget [widget state] "returns state updated.")
  (draw-widget [widget state] "returns nothing.  No state change.")
  )

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
            child-path (concat path [child-tag])
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

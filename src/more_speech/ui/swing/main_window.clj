(ns more-speech.ui.swing.main-window
  (:require [clojure.core.async :as async]
            [more-speech.ui.formatters :as formatters]
            [more-speech.nostr.events :as events]
            [more-speech.ui.config :as config])
  (:use [seesaw core font tree])
  (:import [javax.swing.tree
            DefaultMutableTreeNode
            DefaultTreeModel
            TreePath]
           (javax.swing Timer)))

(declare add-event)
(defrecord seesawHandler []
  events/event-handler
  (events/handle-text-event [_handler event]
    (invoke-later (add-event event))))

(def ui-context (atom nil))

(declare find-header-node add-references)
(defn add-event [event]
  (let [frame (:frame @ui-context)
        event-id (:id event)
        tree (select frame [:#header-tree])
        model (config tree :model)
        root (.getRoot model)
        child-count (.getChildCount root)
        child (DefaultMutableTreeNode. event-id)]
    (.insertNodeInto model child root child-count)
    (.makeVisible tree (TreePath. (.getPath child)))
    ;(add-references root event)
    ))

(defn add-references [root event]
  (loop [references (events/get-references event)]
    (if (empty? references)
      nil
      (let [reference (first references)
            node (find-header-node root reference)]
        (when (some? node)
          (.add ^DefaultMutableTreeNode node
                (DefaultMutableTreeNode. (:id event))))
        (recur (rest references))))))

(defn find-header-node [root id]
  (loop [children (enumeration-seq (.children root))]
    (let [child (first children)]
      (cond
        (empty? children)
        nil

        (= id (.getUserObject child))
        child

        :else
        (let [found-child (find-header-node child id)]
          (if (some? found-child)
            found-child
            (recur (rest children)))
          ))
      )))

(declare display-jframe
         render-event)
(defn setup-jframe [event-agent]
  (invoke-now (display-jframe event-agent))
  (->seesawHandler))

(declare make-edit-window
         timer-action)
(defn display-jframe [event-agent]
  (let [main-frame (frame :title "More Speech" :size [1000 :by 1000])
        article-area (text :multi-line? true
                           :font config/default-font
                           :editable? false)
        header-tree (tree :renderer (partial render-event event-agent)
                          :root-visible? false
                          :model (DefaultTreeModel. (DefaultMutableTreeNode. "Empty"))
                          :id :header-tree)
        reply-button (button :text "Reply")
        create-button (button :text "Create")
        timer (Timer. 100 nil)]
    (reset! ui-context {:frame main-frame :event-agent event-agent})

    (listen timer :action timer-action)

    (listen main-frame :window-closing
            (fn [_]
              (.stop timer)
              (async/>!! (:send-chan @event-agent)
                         [:closed])
              (.dispose main-frame)))

    (listen header-tree :selection
            (fn [e]
              (when (last (selection e))
                (let [selected-id (.getUserObject (last (selection e)))
                      event-state @event-agent
                      text-map (:text-event-map event-state)
                      event (get text-map selected-id)]
                  (text! article-area (formatters/format-article event-state event))))))

    (listen reply-button :action
            (fn [_]
              (make-edit-window :reply event-agent header-tree)))

    (listen create-button :action
            (fn [_] (make-edit-window :send event-agent nil)))

    (config! main-frame :content (border-panel
                                   :north (scrollable header-tree)
                                   :center (scrollable article-area)
                                   :south (flow-panel :items [reply-button create-button])))
    (show! main-frame)
    (.start timer)))

(defn timer-action [_]
  ;nothing for now.
  )

(defn render-event [event-agent widget info]
  (config! widget :font config/default-font)
  (if (seqable? (:value info))
    (text! widget "Articles")
    (let [event-state @event-agent
          nicknames (:nicknames event-state)
          event-map (:text-event-map event-state)
          node (:value info)
          event-id (.getUserObject node)
          event (get event-map event-id)]
      (text! widget (formatters/format-header nicknames event)))
    ))

(defn make-edit-window [kind event-agent header-tree]
  (let [reply? (= kind :reply)
        event-state @event-agent
        edit-frame (frame :title (name kind)
                          :size [1000 :by 500]
                          :on-close :dispose)
        edit-area (text :multi-line? true
                        :font config/default-font)
        send-button (button :text "Send")
        event-map (:text-event-map event-state)
        selected-id (if reply?
                      (.getUserObject (last (selection header-tree)))
                      nil)
        event (if reply?
                (get event-map selected-id)
                nil)]
    (listen send-button :action
            (fn [_]
              (let [message (text edit-area)]
                (events/send-msg event-state event message))
              (dispose! edit-frame)))
    (text! edit-area
           (if reply?
             (formatters/format-reply event)
             ""))
    (config! edit-frame :content
             (border-panel
               :center (scrollable edit-area)
               :south (flow-panel :items [send-button])))
    (show! edit-frame)))

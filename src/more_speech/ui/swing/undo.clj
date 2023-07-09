(ns more-speech.ui.swing.undo
  (:import (java.awt.event KeyEvent)
           (javax.swing AbstractAction Action KeyStroke)
           (javax.swing.event UndoableEditListener)
           (javax.swing.undo UndoManager)))
(defn update-undo [undo-action undo-manager]
  (if (.canUndo undo-manager)
    (do
      (.setEnabled undo-action true)
      (.putValue undo-action Action/NAME (.getUndoPresentationName undo-manager)))
    (do
      (.setEnabled undo-action false)
      (.putValue undo-action Action/NAME "Undo"))))

(defn update-redo [redo-action undo-manager]
  (if (.canUndo undo-manager)
    (do
      (.setEnabled redo-action true)
      (.putValue redo-action Action/NAME (.getRedoPresentationName undo-manager)))
    (do
      (.setEnabled redo-action false)
      (.putValue redo-action Action/NAME "Redo"))))

(defn make-undo-handler [undo-manager undo-action redo-action]
  (proxy [UndoableEditListener] []
    (undoableEditHappened [e]
      (.addEdit undo-manager (.getEdit e))
      (update-undo undo-action undo-manager)
      (update-redo redo-action undo-manager))))

(defn make-undo-action [undo-manager actions]
  (let [undo-action (proxy [AbstractAction] ["Undo"]
                      (actionPerformed [_e]
                        (.undo undo-manager)
                        (update-undo this undo-manager)
                        (update-redo (:redo-action @actions) undo-manager))
                      )]
    (.setEnabled undo-action false)
    undo-action))

(defn make-redo-action [undo-manager actions]
  (let [redo-action (proxy [AbstractAction] ["Redo"]
                      (actionPerformed [_e]
                        (.redo undo-manager)
                        (update-redo this undo-manager)
                        (update-undo (:undo-action @actions) undo-manager))
                      )]
    (.setEnabled redo-action false)
    redo-action))

(defn setup-undo [text-component]
  (let [actions (atom nil)
        document (.getDocument text-component)
        undo-manager (UndoManager.)
        undo-action (make-undo-action undo-manager actions)
        redo-action (make-redo-action undo-manager actions)
        undo-handler (make-undo-handler undo-manager undo-action redo-action)
        undo-key (KeyStroke/getKeyStroke KeyEvent/VK_Z KeyEvent/META_DOWN_MASK)
        redo-key (KeyStroke/getKeyStroke KeyEvent/VK_Y KeyEvent/META_DOWN_MASK)
        input-map (.getInputMap text-component)
        action-map (.getActionMap text-component)]
    (reset! actions {:undo-action undo-action
                     :redo-action redo-action})
    (.addUndoableEditListener document undo-handler)
    (.put input-map undo-key "undoKeystroke")
    (.put action-map "undoKeystroke" undo-action)
    (.put input-map redo-key "redoKeystroke")
    (.put action-map "redoKeystroke" redo-action)
    )
  )

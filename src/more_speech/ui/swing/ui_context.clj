(ns more-speech.ui.swing.ui-context)

(def ui-context (atom {:frame nil
                       :event-agent nil
                       :node-map {}
                       :orphaned-references {}
                       :selected-tab nil}))

(ns more-speech.ui.swing.user-info-interface)

; one implementation to break cycle.
(defmulti show-user-profile (fn [_id] :user-info))

(ns more-speech.user-configuration
  (:require [more-speech.ui.swing.ui-context :refer :all]))

(defn validate-export-user-profile [user-configuration]
  (let [xup (get user-configuration :export-user-profile {})
        xad (get xup :export-after-days 7)
        lte (get xup :last-time-exported 0)
        xup (assoc xup :export-after-days xad
                       :last-time-exported lte)]
    (assoc user-configuration :export-user-profile xup))
  )

(defn validate [user-configuration]
  (let [valid-config (validate-export-user-profile user-configuration)]
    valid-config))


(defn should-export? [now-in-seconds]
  (let [user-configuration (:user-configuration @(:event-context @ui-context))
        xad (get-in user-configuration [:export-user-profile :export-after-days])
        lte (get-in user-configuration [:export-user-profile :last-time-exported])
        export-after-seconds (* 86400 xad)]
    (>= now-in-seconds (+ lte export-after-seconds)))
  )

(defn set-last-time-exported [export-time]
  (let [event-context (:event-context @ui-context)]
    (swap! event-context
           assoc-in
           [:user-configuration :export-user-profile :last-time-exported]
           export-time)))
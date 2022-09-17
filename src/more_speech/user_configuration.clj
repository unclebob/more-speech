(ns more-speech.user-configuration
  (:require [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.config :as config]))

(defn validate-export-user-profile [user-configuration]
  (let [xup (get user-configuration :export-user-profile {})
        xad (get xup :export-after-days 7)
        lte (get xup :last-time-exported 0)
        xup (assoc xup :export-after-days xad
                       :last-time-exported lte)]
    (assoc user-configuration :export-user-profile xup))
  )

(defn validate-import-metadata [uc]
  (let [im (get uc :import-metadata {})
        iad (get im :import-after-days 30)
        lti (get im :last-time-imported 0)
        im (assoc im :import-after-days iad
                     :last-time-imported lti)]
    (assoc uc :import-metadata im)))

(defn validate [user-configuration]
  (-> user-configuration
      validate-export-user-profile
      validate-import-metadata)
  )

(defn keys-last-modified []
  (let [keys-file (clojure.java.io/file @config/keys-filename)]
    (quot (.lastModified keys-file) 1000)))

(defn should-export-profile? [now-in-seconds]
  (let [user-configuration (get-event-state :user-configuration)
        xad (get-in user-configuration [:export-user-profile :export-after-days])
        lte (get-in user-configuration [:export-user-profile :last-time-exported])]
    (and (number? xad)
         (or (>= now-in-seconds (+ lte (* 86400 xad)))
             (>= (keys-last-modified) lte))))
  )

(defn set-last-time-profile-exported [export-time]
  (let [event-context (:event-context @ui-context)]
    (swap! event-context
           assoc-in
           [:user-configuration :export-user-profile :last-time-exported]
           export-time)))

(defn should-import-metadata? [now-in-seconds]
  (let [user-configuration (get-event-state :user-configuration)
        iad (get-in user-configuration [:import-metadata :import-after-days])
        lti (get-in user-configuration [:import-metadata :last-time-imported])]
    (and (number? iad)
         (>= now-in-seconds (+ lti (* 86400 iad))))))

(defn set-last-time-metadata-imported [import-time]
  (let [event-context (:event-context @ui-context)]
    (swap! event-context
           assoc-in
           [:user-configuration :import-metadata :last-time-imported]
           import-time)))
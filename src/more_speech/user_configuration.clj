(ns more-speech.user-configuration
  (:require [more-speech.mem :refer :all]
            [more-speech.config :as config]))

(defn get-config
  ([]
   (get-mem :user-configuration))
  ([tag]
   (get-config tag nil))
  ([tag default]
   (if (coll? tag)
     (get-in (get-config) tag default)
     (get (get-mem :user-configuration) tag default))))

(defn set-config [tag value]
  (if (coll? tag)
    (swap! (get-mem) assoc-in (concat [:user-configuration] tag) value)
    (swap! (get-mem) assoc-in [:user-configuration tag] value)))

(defn validate-export-user-profile [user-configuration]
  (let [xup (get user-configuration :export-user-profile {})
        xad (get xup :export-after-days 7)
        lte (get xup :last-time-exported 0)
        xup (assoc xup :export-after-days xad
                       :last-time-exported lte)]
    (assoc user-configuration :export-user-profile xup)))

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
      validate-import-metadata))

(defn keys-last-modified []
  (let [keys-file (clojure.java.io/file @config/keys-filename)]
    (quot (.lastModified keys-file) 1000)))

(defn should-export-profile? [now-in-seconds]
  (let [xad (get-config [:export-user-profile :export-after-days])
        lte (get-config [:export-user-profile :last-time-exported])]
    (and (number? xad)
         (or (>= now-in-seconds (+ lte (* 86400 xad)))
             (>= (keys-last-modified) lte))))
  )

(defn set-last-time-profile-exported [export-time]
  (set-config [:export-user-profile :last-time-exported] export-time))

(defn should-import-metadata? [now-in-seconds]
  (let [iad (get-config [:import-metadata :import-after-days])
        lti (get-config [:import-metadata :last-time-imported])]
    (and (number? iad)
         (>= now-in-seconds (+ lti (* 86400 iad))))))

(defn set-last-time-metadata-imported [import-time]
  (set-config [:import-metadata :last-time-imported] import-time))

(defn get-default-font []
  (get-config :default-font config/default-font))

(defn get-bold-font []
  (get-config :bold-font config/bold-font))

(defn get-small-font []
  (get-config :small-font config/small-font))
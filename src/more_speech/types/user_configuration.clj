(ns more-speech.types.user-configuration
  (:require [clojure.spec.alpha :as s]))



(s/def ::font string?)
(s/def ::time pos-int?)
(s/def ::default-font ::font)
(s/def ::bold-font ::font)
(s/def ::small-font ::font)
(s/def ::export-after-days (s/or :exporting pos-int?
                                 :not-exporting #(= :never %)))

(s/def ::import-after-days (s/or :importing pos-int?
                                 :not-importing #(= :never %)))

(s/def ::last-time-exported ::time)
(s/def ::last-time-imported ::time)

(s/def ::export-user-profile
  (s/keys :opt-un [::export-after-days
                   ::last-time-exported]))

(s/def ::import-metadata
  (s/keys :opt-un [::import-after-days
                   ::last-time-imported]))

(s/def ::user-configuration
  (s/keys :req-un [::export-user-profile
                   ::import-metadata
                   ::default-font
                   ::bold-font
                   ::small-font]))
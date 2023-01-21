(ns more-speech.db.gateway)

(defmulti add-profile (fn [db _id _profile] (::type db)))
(defmulti add-text-event (fn [db _id _event] (::type db)))

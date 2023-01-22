(ns more-speech.db.gateway)

(defmulti add-profile (fn [db _id _profile] (::type db)))
(defmulti add-text-event (fn [db _id _event] (::type db)))
(defmulti event-exists? (fn [db _id] (::type db)))
(defmulti add-event (fn [db _id _event] (::type db)))
(defmulti add-relays-to-event (fn [db _id _relays] (::type db)))
(defmulti add-reference-to-event (fn [db _id _reference] (::type db)))


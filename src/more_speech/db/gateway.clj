(ns more-speech.db.gateway)

(defmulti add-profile (fn [db _id _profile] (::type db)))
(defmulti get-profile (fn [db _id] (::type db)))
(defmulti event-exists? (fn [db _id] (::type db)))
(defmulti add-event (fn [db _id _event] (::type db)))
(defmulti add-events (fn [db _events] (::type db)))
(defmulti get-event (fn [db _id] (::type db)))
(defmulti get-event-ids-since (fn [db _start-time] (::type db)))
(defmulti update-event-as-read (fn [db _id] (::type db)))
(defmulti add-relays-to-event (fn [db _id _relays] (::type db)))
(defmulti add-reference-to-event (fn [db _id _reference] (::type db)))
(defmulti add-contacts (fn [db _user-id _contacts] (::type db)))
(defmulti get-contacts (fn [db _user-id] (::type db)))
(defmulti get-id-from-username (fn [db _user-name] (::type db)))



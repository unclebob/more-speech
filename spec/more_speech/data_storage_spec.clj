(ns more-speech.data-storage-spec
  (:require [speclj.core :refer :all]
            [more-speech.data-storage-imp :refer :all]))

(defn- created-at [day n]
  (+ n (* day 86400)))

(describe "data-storage"
  (context "partition-messages-by-day"
    (it "partitions messages by day"
      (let [m1 {:id 1 :created-at (created-at 0 0)}
            m2 {:id 2 :created-at (created-at 0 1)}
            m3 {:id 3 :created-at (created-at 1 0)}
            m4 {:id 4 :created-at (created-at 1 1)}
            m5 {:id 5 :created-at (created-at 2 0)}
            m6 {:id 6 :created-at (created-at 2 1)}
            messages {1 m1
                      2 m2
                      3 m3
                      4 m4
                      5 m5
                      6 m6
                      }
            messages-by-day (partition-messages-by-day messages)]
        (should= [[0 [m1 m2]]
                  [1 [m3 m4]]
                  [2 [m5 m6]]]
                 messages-by-day)))

    (it "calculates file name from day"
      (should= "0-01Jan70" (file-name-from-day 0))
      (should= "19192-19Jul22" (file-name-from-day (quot 1658241430 86400)))
      )
    )

  (context "time-from-file-name"
    (it "should get the time from a file name"
      (should= 0 (time-from-file-name "0-01Jan70"))
      (should= (* 86400 19192) (time-from-file-name "19192-19Jul22"))
      (should-be-nil (time-from-file-name "bad-file-name"))
      (should-be-nil (time-from-file-name nil))))

  (context "is-message-file?"
    (it "recognizes message files by name"
      (should (is-message-file? "19192-19Jul22"))
      (should-not (is-message-file? "bad-file-name"))))
  )

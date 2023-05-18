(ns more-speech.util.fortune-messages-spec
  (:require [speclj.core :refer :all]
            [more-speech.util.fortune-messages :refer :all]
            [more-speech.util.fortune-words :refer :all]))

(describe "fortune"
  (it "gets random fortunes"
    (pending "just for fun")
    (doseq [_n (range 100)] (prn (get-markov-message :insane)))))

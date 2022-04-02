(ns more-speech.ui.edit-window-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.edit-window :refer :all]
            )
  )

(describe "edit-window"
  (context "adding characters"
    (it "creates text field if none exists"
      (let [frame {}
            frame (add-char frame \a)]
        (should= ["a"] (:text frame))
        (should= [1 0] (:insertion-point frame))))

    (it "appends to text field if exists."
      (let [frame {:text ["a"]
                   :insertion-point [1 0]}
            frame (add-char frame \a)]
        (should= ["aa"] (:text frame))
        (should= [2 0] (:insertion-point frame))))

    (it "inserts into first line of text field"
      (let [frame {:text ["_____"]
                   :insertion-point [3 0]}
            frame (add-char frame \i)]
        (should= ["___i__"] (:text frame))
        (should= [4 0] (:insertion-point frame)))
      )

    (it "inserts into second line of text field"
      (let [frame {:text ["_____" "_____"]
                   :insertion-point [3 1]}
            frame (add-char frame \i)]
        (should= ["_____" "___i__"] (:text frame))
        (should= [4 1] (:insertion-point frame)))
      )

    (it "appends new lines"
      (let [frame {:text ["a"]
                   :insertion-point [1 0]}
            frame (add-char frame \newline)]
        (should= ["a" ""] (:text frame))
        (should= [0 1] (:insertion-point frame))))

    (it "prepends new lines"
          (let [frame {:text ["a"]
                       :insertion-point [0 0]}
                frame (add-char frame \newline)]
            (should= ["" "a"] (:text frame))
            (should= [0 1] (:insertion-point frame))))

    (it "inserts new lines"
          (let [frame {:text ["a" "_____" "b"]
                       :insertion-point [3 1]}
                frame (add-char frame \newline)]
            (should= ["a" "___" "__" "b"] (:text frame))
            (should= [0 2] (:insertion-point frame))))
    )
  )

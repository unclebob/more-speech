(ns more-speech.ui.app-util)

(def update-articles-path [:application :update-articles])
(def mouse-lock-path [:application :mouse-locked-to])

(defn update-articles [state]
  (assoc-in state update-articles-path true))

(defn articles-updated [state]
  (assoc-in state update-articles-path false))

(defn update-articles? [state]
  (get-in state update-articles-path true))

(defn lock-mouse [state widget]
  (assoc-in state mouse-lock-path (:path widget)))

(defn unlock-mouse [state]
  (assoc-in state mouse-lock-path nil))

(defn get-mouse-lock [state]
  "returns path of locked widget or nil."
  (get-in state mouse-lock-path nil))
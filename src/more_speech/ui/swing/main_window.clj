(ns more-speech.ui.swing.main-window
  (:require [clojure.core.async :as async]
            [more-speech.nostr.util :as util]
            [more-speech.ui.formatters :as formatters]
            [more-speech.nostr.events :as events])
  (:import (javax.swing JFrame SwingUtilities Timer JScrollPane JTextPane)
           (java.awt BorderLayout Toolkit Font)
           (java.awt.event ActionListener WindowListener)))

(declare display-jframe action-event draw-events)

(defn setup-jframe [event-agent output-channel]
  (SwingUtilities/invokeLater #(display-jframe event-agent output-channel)))

(defmacro with-action [component event & body]
  `(.addActionListener ~component
                       (proxy [ActionListener] []
                         (actionPerformed [~event] ~@body))))

(defn display-jframe [event-agent output-channel]
  (let [frame (JFrame. "More Speech")
        text-area (doto
                    (JTextPane.)
                    (.setContentType "text/html")
                    (.setText "<br><br><br><br><br><br><br><br><br><br>"))
        scroll-pane (JScrollPane. text-area)
        content (.getContentPane frame)
        timer (Timer. 100 nil)
        screen-size (.getScreenSize (Toolkit/getDefaultToolkit))
        font (Font. "Courier New" Font/PLAIN, 14)]

    (.addWindowListener frame (proxy [WindowListener] []
                                (windowActivated [_e])
                                (windowOpened [_e])
                                (windowClosed [_e])
                                (windowDeactivated [_e])
                                (windowClosing [_e]
                                  (.stop timer)
                                  (async/>!! output-channel [:closed])
                                  (.dispose frame))))

    (with-action timer action-event
                 (draw-events text-area event-agent)
                 )
    (.setSize frame (.getWidth screen-size) (.getHeight screen-size))
    (.setSize text-area (.getWidth screen-size) (.getHeight screen-size))
    (.setFont text-area font)
    (.add content scroll-pane BorderLayout/NORTH)
    (.setVisible frame true)
    (.start timer)))

(declare format-events append-event format-event)

(defn draw-events [text-area event-agent]
  (prn "tick")
  (when (:update @event-agent)
    (let [event-state @event-agent
          formatted-events (format-events event-state)]
      (.setText text-area formatted-events)
      (send event-agent events/updated))))

(defn format-events [{:keys [chronological-text-events nicknames text-event-map]}]
  (let [header-ids (map first chronological-text-events)
        headers (map #(get text-event-map %) header-ids)]
    (reduce #(append-event nicknames %1 %2) "" headers)))

(defn append-event [nicknames formatted-events event]
  (str formatted-events (format-event nicknames event)))

(defn format-event [nicknames {:keys [pubkey created-at content]}]
  (str "<p>"
       (formatters/abbreviate
         (get nicknames pubkey (util/num->hex-string pubkey))
         20)
       " "
       (formatters/format-time created-at)
       " "
       (formatters/abbreviate content 50)
       "</b><<hr>"))
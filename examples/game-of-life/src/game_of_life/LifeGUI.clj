(ns game-of-life.LifeGUI
  (:gen-class
   :extends javax.swing.JApplet
   :state state
   :prefix "-"
   :methods [[startActions [] void]])
  (:require [game-of-life.utils :as utils])
  (:import [java.awt Color Graphics Graphics2D]
           [javax.swing Timer AbstractAction]
           [java.awt.event ActionListener]
           [java.awt.geom Rectangle2D$Double]
           javax.swing.JApplet))

(def universe-width 96)
(def update-universe-delay 64)
(def universe (atom (utils/gen-universe universe-width universe-width)))

(defn -init
  [#^JApplet applet]
  (let [bg (Color/WHITE)
        fg (Color/black)]
    (doto applet
      (.setBackground bg)
      (.setForeground fg))))

(defn -paint
  [#^JApplet applet #^Graphics g]
  (let [g2 (cast Graphics2D g)
        d (.getSize applet)
        rec-width (/ (.width d) universe-width)
        rec-height (/ (.height d) universe-width)]
    (doall
     (for [row (range 0 universe-width)
           :let [y (* row rec-height)]]
       (doall
        (for [col (range 0 universe-width)
              :let [x (* col rec-width)]]
          (do
            (if (= (utils/find-pos @universe row col) 0)
              (.setPaint g2 Color/WHITE)
              (.setPaint g2 Color/black))
            (.fill g2 (new Rectangle2D$Double x y rec-width rec-height)))))))))


(defn -startActions
  [#^JApplet applet]
  (let [updateUniverseAction (proxy [AbstractAction ActionListener] []
                               (actionPerformed [e]
                                 (->> @universe
                                      utils/update-universe
                                      (reset! universe))
                                 (.repaint applet)))]
    (.start (Timer. update-universe-delay updateUniverseAction))))

(ns game-of-life.core
  (:import game_of_life.LifeGUI
           java.awt.Dimension
           java.awt.event.WindowAdapter
           javax.swing.JFrame))

(defn -main []
  (let [applet-width 1024
        frame (JFrame. "Conway's Game of Life")
        life-gui (game_of_life.LifeGUI.)]
    (doto frame
      (.addWindowListener
       (proxy [WindowAdapter] []
         (windowClosing [evt]
           (System/exit 0))))
      (.add "Center" life-gui)
      (.pack)
      (.setSize (Dimension. applet-width applet-width))
      (.setVisible true))
    (.startActions life-gui)))

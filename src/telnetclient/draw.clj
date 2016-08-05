(ns telnetclient.draw
  (:require [lanterna.terminal :as t]))


(defn draw-rect
  ([scr x y w h c]
   (doseq [yy (range h)]
     (t/put-string scr (apply str (repeat w c)) x (+ y yy))))
  ([scr x y w h c fg bg]
   (t/set-fg-color scr fg)
   (t/set-bg-color scr bg)
   (draw-rect scr x y w h c)
   (t/set-fg-color scr :default)
   (t/set-bg-color scr :default)))

(defn draw-border [scr x y w h c fg bg]
  (t/set-fg-color scr fg)
  (t/set-bg-color scr bg)
  (draw-rect scr x y w 1 c)
  (draw-rect scr x (+ y h -1) w 1 c)
  (draw-rect scr x y 1 h c)
  (draw-rect scr (+ x w -1) y 1 h c)
  (t/set-fg-color scr :default)
  (t/set-bg-color scr :default))

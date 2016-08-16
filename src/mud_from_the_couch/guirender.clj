(ns mud-from-the-couch.guirender
  (:require [lanterna.terminal :as t]
            [mud-from-the-couch.draw :refer [draw-border draw-rect]]))

(defn render-messagebox [scr x y w h borderbg fillbg fillfg msg]
  (draw-border scr x y w h \. :default borderbg)
  (draw-rect scr (inc x) (inc y) (- w 2) (- h 2) \space :default fillbg)
  (t/set-bg-color scr fillbg)
  (t/set-fg-color scr fillfg)
  (t/put-string scr msg (+ x 2) (+ y 2))
  (t/set-bg-color scr :default)
  (t/set-fg-color scr :default))

(defn render-statusbox [scr x y w h borderbg fillbg fillfg info]
  (draw-border scr x y w h \. :default borderbg)
  (draw-rect scr (inc x) (inc y) (- w 2) (- h 2) \space :default fillbg)
  (t/set-bg-color scr fillbg)
  (t/set-fg-color scr fillfg)  
  (let [{:keys [hp max-hp mv max-mv pos]} info]
    (t/put-string scr (str "HP: " hp " / " max-hp) (+ x 2) (+ y 2))
    (t/put-string scr (str "MOVES: " mv " / " max-mv) (+ x 2) (+ y 4))
    (t/put-string scr (str "POS: " pos) (+ x 2) (+ y 6)))
  (t/set-bg-color scr :default)
  (t/set-fg-color scr :default))

(defn render-exitsbox [scr x y w h borderbg fillbg fillfg info]
  (draw-border scr x y w h \. :default borderbg)
  (draw-rect scr (inc x) (inc y) (- w 2) (- h 2) \space :default fillbg)
  (t/set-bg-color scr fillbg)
  (t/set-fg-color scr fillfg)  
  (t/put-string scr "@" (+ x 11) (+ y 5))
  (t/put-string scr "------" (+ x 23) (+ y 5))
  (dorun (map #(case %
                 "Northwest" (t/put-string scr "NWest" (+ x 2) (+ y 2))
                 "North" (t/put-string scr % (+ x 9) (+ y 2))
                 "Northeast" (t/put-string scr "NEast" (+ x 16) (+ y 2))
                 "West" (t/put-string scr % (+ x 2) (+ y 5))
                 "East" (t/put-string scr % (+ x 16) (+ y 5))
                 "Southwest" (t/put-string scr "SWest" (+ x 2) (+ y 8))
                 "South" (t/put-string scr % (+ x 9) (+ y 8))
                 "Southeast" (t/put-string scr "SEast" (+ x 16) (+ y 8))
                 "Up" (t/put-string scr % (+ x 25) (+ y 3))
                 "Down" (t/put-string scr % (+ x 24) (+ y 7)))
              (:exits info)))
  (t/set-bg-color scr :default)
  (t/set-fg-color scr :default))


(defn render-selected-objectbox [scr x y w h borderbg fillbg fillfg object word-index]
  (draw-border scr x y w h \. :default borderbg)
  (draw-rect scr (inc x) (inc y) (- w 2) (- h 2) \space :default fillbg)
  (t/set-bg-color scr fillbg)
  (t/set-fg-color scr fillfg)
  (t/move-cursor scr (+ x 2) (+ y 2))
  (dorun (map-indexed
          (fn [ind word] (if (= ind word-index)
                           (do 
                             (t/put-string scr "[[")
                             (t/set-bg-color scr :green)
                             (t/put-string scr word)
                             (t/set-bg-color scr fillbg)
                             (t/put-string scr "]] "))
                           (t/put-string scr (str word " "))))
          object))
  (t/set-bg-color scr :default)
  (t/set-fg-color scr :default))


(defn render-objectsbox [scr x y w h borderbg fillbg fillfg objects sel-ind]
  (draw-rect scr (inc x) (inc y) (- w 2) (- h 2) \space :default fillbg)
  (t/set-bg-color scr fillbg)
  (t/set-fg-color scr fillfg)
  (let [obj-to-print (map #(subs % 0 (min 50 (count %))) objects)]
    (dorun (map-indexed
            (fn [ind obj]
              (let [col-num (quot ind (- h 2))
                    xx (+ x (inc (* col-num (quot w 2))))
                    yy (+ y 1 (case col-num
                                0 ind
                                1 (rem ind (- h 2))))]
                (if (= ind sel-ind)
                  (do
                    (t/set-bg-color scr :green)
                    (t/put-string scr obj xx yy)
                    (t/set-bg-color scr fillbg))
                  (t/put-string scr obj xx yy))))
            obj-to-print)))
  (t/set-bg-color scr :default)
  (t/set-fg-color scr :default)
  (draw-rect scr (+ x (quot w 2)) y 1 h \. :default borderbg)
  (draw-border scr x y w h \. :default borderbg))



(ns mud-from-the-couch.controller
  (:require [clojure.core.async :refer [<! >! <!! >!! chan go close!]])
  (:import [net.java.games.input ControllerEnvironment]))

(def gamepad-chan (chan))
(def os-type (atom nil))


(def windows-64bits-button-translate
  {"X Axis" :left-x
   "Y Axis" :left-y
   "Z Axis" :trigger                    ;Left trigger +ve 1, right trigger -ve 1
   "X Rotation" :right-x
   "Y Rotation" :right-y
   "Hat Switch" :dir-pad
   "Button 0" :A
   "Button 1" :B
   "Button 2" :X
   "Button 3" :Y
   "Button 4" :left-bumper
   "Button 5" :right-bumper
   "Button 6" :back
   "Button 7" :start
   ;; "Mode" :xbox-guide
   "Button 8" :left-thumb
   "Button 9" :right-thumb})


(def linux-64bits-button-translate
  {"x" :left-x
   "y" :left-y
   "z" :left-trigger
   "rx" :right-x
   "ry" :right-y
   "rz" :right-trigger
   "pov" :dir-pad
   "A" :A
   "B" :B
   "X" :X
   "Y" :Y
   "Left Thumb" :left-bumper
   "Right Thumb" :right-bumper
   "Select" :back
   "Unknown" :start
   "Mode" :xbox-guide
   "Left Thumb 3" :left-thumb
   "Right Thumb 3" :right-thumb})

(def bin-comp-name-linux-64bits
  [:A :B :X :Y :left-bumper :right-bumper :back
   :start :xbox-guide :left-thumb :right-thumb])

(def bin-comp-name-windows-64bits
  [:A :B :X :Y :left-bumper :right-bumper :back
   :start :left-thumb :right-thumb])

(def bin-comp-right-extra-map
  {:A :rxA
   :B :rxB
   :X :rxX
   :Y :rxY
   :left-bumper :rxleft-bumper
   :right-bumper :rxright-bumper
   :back :rxback
   :start :rxstart
   :xbox-guide :rxxbox-guide
   :left-thumb :rxleft-thumb
   :right-thumb :rxright-thumb
   })

(defn find-controller []
  (let [controllers (.getControllers (ControllerEnvironment/getDefaultEnvironment))] 
    (let [found-controller (first (filter #(or (= "Controller (XBOX 360 For Windows)" (.getName %))
                                               (= "Microsoft X-Box 360 pad" (.getName %))) controllers))
          controller-name (.getName found-controller)]
      (case controller-name
        "Controller (XBOX 360 For Windows)" (reset! os-type :windows-64bits)
        "Microsoft X-Box 360 pad" (reset! os-type :linux-64bits))
      found-controller)))


(defn get-poll-data [comp-name components]
  (let [comp-map (zipmap comp-name (map #(.getPollData %) components))]
    (if (= @os-type :windows-64bits)
      (as-> comp-map m
          (assoc m :left-trigger (m :trigger))
          (assoc m :right-trigger (- (m :trigger))))
      comp-map)))

(defn gamepad-jinput []
  (let [controller (find-controller)
        components (.getComponents controller)
        comp-name (case @os-type
                    :linux-64bits (replace linux-64bits-button-translate (map #(.getName %) components))
                    :windows-64bits (replace windows-64bits-button-translate (map #(.getName %) components)))]
    (.poll controller)
    (loop [prev-comp-map (get-poll-data comp-name components)]
      (.poll controller)
      (let [curr-comp-map (get-poll-data comp-name components)]
        (doseq [button (case @os-type
                         :linux-64bits bin-comp-name-linux-64bits
                         :windows-64bits bin-comp-name-linux-64bits)]
          (if (and (= (prev-comp-map button) 0.0)
                   (= (curr-comp-map button) 1.0))
            (if (< (curr-comp-map :right-trigger) 0.5)
              (>!! gamepad-chan button)
              (>!! gamepad-chan (bin-comp-right-extra-map button)))))
        (if (= (prev-comp-map :dir-pad) 0.0)
          (case (int (* 1000 (curr-comp-map :dir-pad)))
            250 (>!! gamepad-chan :U)
            750 (>!! gamepad-chan :D)
            nil))
        (if (< (Math/hypot (prev-comp-map :left-y)
                           (prev-comp-map :left-x)) 0.5) 
          (let [curr-x (curr-comp-map :left-x)
                curr-y (curr-comp-map :left-y)
                r (Math/hypot curr-x curr-y)
                angle (+ 180 (Math/toDegrees (Math/atan2 curr-y curr-x)))]
            (if (>= r 0.5)
              (cond
                (< angle 22.5) (>!! gamepad-chan :W)
                (< angle 67.5) (>!! gamepad-chan :NW)
                (< angle 112.5) (>!! gamepad-chan :N)
                (< angle 157.5) (>!! gamepad-chan :NE)
                (< angle 202.5) (>!! gamepad-chan :E)
                (< angle 247.5) (>!! gamepad-chan :SE)
                (< angle 292.5) (>!! gamepad-chan :S)
                (< angle 337.5) (>!! gamepad-chan :SW)
                :else (>!! gamepad-chan :W)
                ))))
        (if (and (< (Math/abs (prev-comp-map :right-y)) 0.5)
                 (> (Math/abs (curr-comp-map :right-y)) 0.5))
          (if (pos? (curr-comp-map :right-y))
            (>!! gamepad-chan :right-y-down)
            (>!! gamepad-chan :right-y-up)))
        (if (and (< (Math/abs (prev-comp-map :right-x)) 0.5)
                 (> (Math/abs (curr-comp-map :right-x)) 0.5))
          (if (pos? (curr-comp-map :right-x))
            (>!! gamepad-chan :right-x-right)
            (>!! gamepad-chan :right-x-left)))
        (Thread/sleep 100)
        (recur curr-comp-map)))))



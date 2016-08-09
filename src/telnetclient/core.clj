(ns telnetclient.core
  (:require [lanterna.terminal :as t]
            [telnetclient.guirender :refer [render-messagebox
                                            render-statusbox
                                            render-rawdatabox
                                            render-exitsbox
                                            render-objectsbox
                                            render-selected-objectbox]]
            [clojure.core.async :refer [<! >! <!! >!! chan go close!]])
  (:import [org.apache.commons.net.telnet
            TelnetClient
            TerminalTypeOptionHandler
            EchoOptionHandler
            SuppressGAOptionHandler]
           [net.java.games.input ControllerEnvironment])
  (:gen-class))

(declare parse-cmd cmd-chan term-output prev-obj prev-word next-obj next-word)


;; Async channel
;; =============================================================================
(def screen-chan (chan))
(def key-stroke-chan (chan))
(def cmd-chan (chan))
(def trigger-chan (chan))
(def render-chan (chan))
(def gamepad-chan (chan))


;; App states
;; =============================================================================
(def interface-mode (atom :input-key))
(def os-type (atom nil))
(def running (atom true))
(def key-buffer (atom ""))
(def cmd-buffer (atom {:history '()
                       :pointer 1}))
(def in-buf (byte-array 1024))
(def screen-buffer (atom []))
(def tab-complete-list (atom #{}))
(def key-stroke-buffer (atom '()))
(def log (atom []))
(def session-info (atom {:hp "unknown"
                         :max-hp "unknown"
                         :mv "unknown"
                         :max-mv "unknown"
                         :pos "unknown"
                         :exits []
                         :objects []
                         :select-index 0
                         :curr-object []
                         :word-index 0
                         :target-prefix 1
                         :selected-target ""
                         :binded-cmd {:1 "dragon"} 
                         :binded-gamepad-cmds {:X "#dec-prefix"
                                               :Y "#inc-prefix"
                                               :A "#cmd-1"
                                               :B "#cmd-1-target"}}))
(def client-mods (atom (eval (read-string (slurp "resources/slurp.edn")))))



;; Client config
;; =============================================================================
(def tc
  (doto (TelnetClient.)
    (.addOptionHandler (TerminalTypeOptionHandler. "VT100"
                                                   false false true false))
    (.addOptionHandler (EchoOptionHandler. true false true false))
    (.addOptionHandler (SuppressGAOptionHandler. true true true true))
    ))

(.connect tc "mud.durismud.com" 7777)




;; Streams
;; =============================================================================
(def ips (.getInputStream tc))
(def ops (.getOutputStream tc))




;; Terminals
;; =============================================================================
(def term-input (t/get-terminal
                 :swing
                 {:cols 80
                  :rows 1
                  :palette :gnome}))

(def term-output (t/get-terminal
                  ;; :unix
                  :swing
                  {
                   :cols 160
                   :rows 50
                   :palette :mac-os-x}))



;; Controller
;; =============================================================================

(defn find-controller []
  (let [controllers (.getControllers (ControllerEnvironment/getDefaultEnvironment))] 
    (let [found-controller (first (filter #(or (= "Controller (XBOX 360 For Windows)" (.getName %))
                                               (= "Microsoft X-Box 360 pad" (.getName %))) controllers))
          controller-name (.getName found-controller)]
      (case controller-name
        "Controller (XBOX 360 For Windows)" (reset! os-type :windows-64bits)
        "Microsoft X-Box 360 pad" (reset! os-type :linux-64bits))
      found-controller)))


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

(def bin-comp-name
  [:A :B :X :Y :left-bumper :right-bumper :back
   :start :xbox-guide :left-thumb :right-thumb])

(defn gamepad-jinput [controller]
  (let [components (.getComponents controller)
        comp-name (case @os-type
                    :linux-64bits (replace linux-64bits-button-translate (map #(.getName %) components))
                    :windows-64bits (replace windows-64bits-button-translate (map #(.getName %) components)))]
    (.poll controller)
    (loop [prev-comp-map (zipmap comp-name (map (fn [comp] (.getPollData comp)) components))]
      (.poll controller)
      (let [curr-comp-map (zipmap comp-name (map (fn [comp] (.getPollData comp)) components))]
        ;; (doseq [comp components] (println (.getName comp) "\t" (.getPollData comp)))
        ;; (doseq [comp curr-comp-map] (println comp))
        ;; (println "\n\n")
        (doseq [button bin-comp-name]
          (if (and (= (prev-comp-map button) 0.0)
                   (= (curr-comp-map button) 1.0))
            (>!! gamepad-chan button)))
        (if (= (prev-comp-map :dir-pad) 0.0)
          (case (int (* 1000 (curr-comp-map :dir-pad)))
            125 (>!! gamepad-chan :NW)
            250 (>!! gamepad-chan :N)
            375 (>!! gamepad-chan :NE)
            500 (>!! gamepad-chan :E)
            625 (>!! gamepad-chan :SE)
            750 (>!! gamepad-chan :S)
            875 (>!! gamepad-chan :SW)
            1000 (>!! gamepad-chan :W)
            nil))
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
        (if @running
          (recur curr-comp-map)
          (prn "Stopping gamepad-jinput"))))))

(defn handle-gamepad-cmd []
  (while @running
    (let [cmd (<!! gamepad-chan)]
      (case cmd
        :right-y-down (next-obj)
        :right-y-up (prev-obj)
        :right-x-right (next-word)
        :right-x-left (prev-word)
        :A (parse-cmd (get-in @session-info [:binded-gamepad-cmds :A]))
        :B (parse-cmd (get-in @session-info [:binded-gamepad-cmds :B]))
        :X (parse-cmd (get-in @session-info [:binded-gamepad-cmds :X]))
        :Y (parse-cmd (get-in @session-info [:binded-gamepad-cmds :Y]))
        :left-bumper (parse-cmd "d")
        :right-bumper (parse-cmd "u")
        :back nil
        :start nil
        :xbox-guide nil
        :left-thumb (parse-cmd "l")
        :right-thumb nil
        :NW (parse-cmd "nw")
        :N (parse-cmd "n")
        :NE (parse-cmd "ne")
        :E (parse-cmd "e")
        :SE (parse-cmd "se")
        :S (parse-cmd "s")
        :SW (parse-cmd "sw")
        :W (parse-cmd "w")
        nil)))
  (prn "Stopping handle-gamepad-cmd"))

;; Functions
;; =============================================================================


(defn check-alias [cmd]
  ((@client-mods :alias) cmd))


(defn next-cmd []
  (swap! cmd-buffer update :pointer #(if (< % 0) 0 (dec %)))
  (reset! key-buffer (last (take (@cmd-buffer :pointer)
                                 (@cmd-buffer :history))))
  (t/clear term-input)
  (t/put-string term-input @key-buffer 0 0))


(defn prev-cmd []
  (swap! cmd-buffer update :pointer inc)  
  (reset! key-buffer (last (take (@cmd-buffer :pointer)
                                 (@cmd-buffer :history))))
  (t/clear term-input)
  (t/put-string term-input @key-buffer 0 0))



(defn print-key-buffer []
  (t/clear term-input)
  (t/put-string term-input @key-buffer 0 0))

(defn handle-key-stroked [c]
  (swap! key-buffer str c)
  (print-key-buffer))


(defn key-buffer-replace-last-word [w]
  (as-> @key-buffer kb
       (clojure.string/split kb #" ")
       (pop kb)
       (conj kb w)
       (clojure.string/join " " kb)
       (reset! key-buffer kb)))

(defn remove-ansi-code [text]
  (clojure.string/replace text #"\u001b\[[^m]+m" ""))

(defn tab-complete []
  (let [prev-key (second @key-stroke-buffer)
        search-regex (re-pattern (str "\\b"
                                      (last (clojure.string/split
                                             @key-buffer #" "))
                                      "\\w+"))]
    (if (= prev-key :tab)
      (swap! tab-complete-list (comp set rest))
      (->> (take-last 5000 @screen-buffer)
           ;; (reverse)
           (concat)
           (apply str)
           (remove-ansi-code)
           (re-seq search-regex)
           (set)
           (reset! tab-complete-list)))
    (key-buffer-replace-last-word (first @tab-complete-list))
    (print-key-buffer)))

(defn update-word [func]
  (let [curr-object (@session-info :curr-object)
        curr-ind (func (@session-info :word-index))
        curr-target (curr-object curr-ind)]
    (swap! session-info assoc :word-index curr-ind)
    (swap! session-info assoc :selected-target curr-target)
    (>!! render-chan
         #(render-selected-objectbox term-output
                                     5 5 145 5
                                     :magenta :white :black
                                     curr-object
                                     curr-ind))
    (>!! render-chan
         #(render-messagebox term-output
                             102 0 50 5
                             :red :white :black
                             (str "Target: "
                                  (@session-info :target-prefix)
                                  "."
                                  curr-target)))))

(defn prev-word []
  (update-word dec))

(defn next-word []
  (update-word inc))


(defn prev-obj []
  (swap! session-info update :select-index #(if (> % 0) (dec %) %))
  (swap! session-info assoc
         :curr-object
         (clojure.string/split
          (get-in @session-info
                  [:objects (@session-info :select-index)]) #" "))
  (>!! render-chan
       #(render-objectsbox term-output
                           0 37 102 12
                           :magenta :white :black
                           (@session-info :objects)
                           (@session-info :select-index))))

(defn next-obj []
  (swap! session-info update :select-index #(if (< % 19) (inc %) %))
  (swap! session-info assoc
         :curr-object
         (clojure.string/split
          (get-in @session-info
                  [:objects (@session-info :select-index)]) #" ")) 
  (>!! render-chan
       #(render-objectsbox term-output
                           0 37 102 12
                           :magenta :white :black
                           (@session-info :objects)
                           (@session-info :select-index))))

(defn key-stroke-capturer []
  (t/in-terminal
   term-input
   (while @running
     (let [c (t/get-key-blocking term-input)]
       (swap! key-stroke-buffer conj c)
       (case c
         :escape (reset! interface-mode :input-key)
         :backspace (do (swap! key-buffer
                               (comp (partial apply str) butlast))
                        (t/clear term-input)
                        (t/put-string term-input @key-buffer 0 0))
         :left (case @interface-mode
                 :input-key nil
                 :selection (prev-word))
         :right (case @interface-mode
                  :input-key nil
                  :selection (next-word))
         :up (case @interface-mode
               :input-key (prev-cmd)
               :selection (prev-obj))
         :down (case @interface-mode
                 :input-key (next-cmd)
                 :selection (next-obj))
         :insert nil
         :delete nil
         :home nil
         :end nil
         :page-up nil
         :page-down nil
         :tab (tab-complete)
         :reverse-tab nil
         :enter (do (t/clear term-input)
                    (parse-cmd))
         \space (if-let [alias-cmd (check-alias)]
                  (do (reset! key-buffer (str alias-cmd \space))
                      (t/clear term-input)
                      (t/put-string term-input
                                    (str alias-cmd \space) 0 0))
                  (handle-key-stroked \space))
         (handle-key-stroked c))))
   (prn "Stopping key-stroke-capturer")))



(defn parse-cmd 
  ([] (parse-cmd @key-buffer))
  ([cmd]
   (println "Parsing cmd: " cmd)
   (if-let [alias-cmd (check-alias cmd)]
     (>!! cmd-chan alias-cmd)
     (case cmd
       "#quit" (do
                 (spit "resources/log.txt" (str @log))
                 (reset! running false)
                 (close! screen-chan)
                 (shutdown-agents))
       "#clear" (t/clear term-output)
       "#select" (reset! interface-mode :selection)
       "#cmd-1" (do (println "effected cmd: " (get-in @session-info [:binded-cmd :1])) (>!! cmd-chan (get-in @session-info [:binded-cmd :1])))
       "#cmd-1-target" (do (println (str (get-in @session-info [:binded-cmd :1])
                                         " "
                                         (@session-info :target-prefix)
                                         "."
                                         (@session-info :selected-target)))
                           (>!! cmd-chan (str (get-in @session-info [:binded-cmd :1])
                                              " "
                                              (@session-info :target-prefix)
                                              "."
                                              (@session-info :selected-target))))
       "#inc-prefix" (do (swap! session-info update :target-prefix inc)
                         (let [curr-object (@session-info :curr-object)
                               curr-ind (@session-info :word-index)
                               curr-target (curr-object curr-ind)]
                           (>!! render-chan
                                #(render-messagebox term-output
                                                    102 0 50 5
                                                    :red :white :black
                                                    (str "Target: "
                                                         (@session-info :target-prefix)
                                                         "."
                                                         curr-target)))))
       "#dec-prefix" (do (swap! session-info update :target-prefix dec)
                         (let [curr-object (@session-info :curr-object)
                               curr-ind (@session-info :word-index)
                               curr-target (curr-object curr-ind)]
                           (>!! render-chan
                                #(render-messagebox term-output
                                                    102 0 50 5
                                                    :red :white :black
                                                    (str "Target: "
                                                         (@session-info :target-prefix)
                                                         "."
                                                         curr-target)))))
       "#load" (reset! client-mods (eval
                                    (read-string
                                     (slurp "resources/slurp.edn"))))
       (>!! cmd-chan cmd)))))



(defn server-output-receiver []
  (while @running
    (Thread/sleep 100)
    (loop [recv-str ""]
      (let [j (.available ips)]
        (if (> j 0)
          (let [i (.read ips in-buf 0 (if (> j 1024) 1024 j))] 
            (recur (str recv-str (String. in-buf 0 i))))
          (if (not-empty recv-str)
            (>!! screen-chan recv-str))))))
  (prn "Stopping print-server-output"))


(defn render-runner []
  (render-rawdatabox term-output 0 0 102 37 :blue nil)
  (render-messagebox term-output 102 0 50 5 :red :white :black nil)
  (render-statusbox term-output 102 5 50 9 :green :white :black nil)
  (render-exitsbox term-output 102 14 31 11 :cyan :white :black nil)
  (render-objectsbox term-output 0 37 102 12 :magenta :white :black nil 0)
  (while @running
    (let [render-function (<!! render-chan)]
      (render-function)))
  (prn "Stopping render-runner"))


(defn print-to-screen [buf]
  (>!! render-chan #(render-rawdatabox term-output 0 0 102 37 :blue buf)))


(defn handle-screen-buffer [lines]
  (swap! screen-buffer
         #(->> %2
               (apply conj %1)
               (take-last 5000)
               (vec))
         lines))



(defn server-output-printer []
  (t/in-terminal
   term-output
   (while @running
     (let [recv-str (<!! screen-chan)
           recv-str-split (clojure.string/split-lines recv-str)
           decolored-recv-str (remove-ansi-code recv-str)]
       (handle-screen-buffer recv-str-split)
       (>!! trigger-chan decolored-recv-str)
       (print-to-screen (take-last 40 @screen-buffer)))))
  (prn "Stopping consume-recv-str"))


(defn re-to-act [buf]
  (fn [[re act]]
    (dorun (map act (re-seq re buf)))))


(defn handle-triggers []
  (while @running
    (let [triggers (@client-mods :trigger)
          buf (<!! trigger-chan)]
      (dorun
       (map (re-to-act buf) triggers))))
  (prn "Stopping trigger handlers"))


(defn handle-cmd-history [cmd]
  (swap! cmd-buffer (fn [m]
                      (-> m
                          (update :history conj cmd)
                          (assoc :pointer 0))))
  (reset! key-buffer ""))


(defn cmd-sender []
  (while @running
    (let [cmd (<!! cmd-chan)
          cmd-vec (vec (str cmd \newline))
          out-buf (byte-array (map (comp byte int) cmd-vec))
          i (count cmd-vec)]
      (.write ops out-buf 0 i)
      (.flush ops)
      (handle-cmd-history cmd)))
  (prn "Stopping cmd-sender"))


  


;; Start threads
;; =============================================================================

(defn -main
  [& args]
  (prn "Mud starting..............")
  (future (key-stroke-capturer))
  (future (server-output-receiver))
  (future (server-output-printer))
  (future (cmd-sender))
  (future (handle-triggers))
  (future (gamepad-jinput (find-controller)))
  (future (handle-gamepad-cmd))
  (future (render-runner)))

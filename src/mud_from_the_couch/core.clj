(ns mud-from-the-couch.core
  (:require [lanterna.terminal :as t]
            [seesaw.core :as s]
            [seesaw.dev :as d]
            [seesaw.keystroke :as k]
            [seesaw.keymap :as m]
            [mud-from-the-couch.guirender :refer [render-messagebox
                                                  render-statusbox
                                                  render-exitsbox
                                                  render-objectsbox
                                                  render-selected-objectbox]]
            [mud-from-the-couch.ansicode :refer [generate-markup
                                                 parse-markup
                                                 remove-ansi-escape-code]]
            [mud-from-the-couch.controller :refer [gamepad-jinput
                                                   gamepad-chan]]
            [clojure.core.async :refer [<! >! <!! >!! chan go close!]])
  (:import [org.apache.commons.net.telnet
            TelnetClient
            TerminalTypeOptionHandler
            EchoOptionHandler
            SuppressGAOptionHandler]
           [java.awt.event KeyEvent])
  (:gen-class))

(declare parse-cmd
         cmd-chan
         term-output
         prev-obj
         prev-word
         next-obj
         next-word
         tab-complete)


;; Async channel
;; =============================================================================
(def screen-chan (chan))
(def key-stroke-chan (chan))
(def cmd-chan (chan))
(def trigger-chan (chan))
(def render-chan (chan))


;; App states
;; =============================================================================
(def running (atom true))
(def cmd-buffer (atom {:history '()
                       :pointer 1}))
(def in-buf (byte-array 1024))
(def screen-buffer (atom []))
(def log (atom []))
(def session-state
  (atom
   {:tab-complete-set '() 
    :key-press-history '()
    :mode :input-key
    :hp "unknown"
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
    :binded-cmd {:1 ["dragon"]
                 :2 ["kneel" "spring"]
                 :3 ["chant quiver"]
                 :4 ["chant regen"]
                 :5 ["chant hero"]} 
    :binded-gamepad-cmds {
                          :left-bumper "#dec-prefix"
                          :right-bumper "#inc-prefix"
                          :X "#cmd-1"
                          :Y "#cmd-2"
                          :A "#cmd-3"
                          :B "#cmd-4"
                          :rxX "#cmd-1-target"
                          :rxY "#cmd-2-target"
                          :rxA "#cmd-3-target"
                          :rxB "#cmd-5"
                          }}))
(def client-mods (atom (eval (read-string (slurp "resources/slurp.edn")))))


;; Seesaw
;; =============================================================================

(s/native!)

(def colors {:black "#303030"
             :red :red
             :green :green
             :yellow :yellow
             :blue :blue
             :magenta :magenta
             :cyan :cyan
             :white :white})
(def background-colors {:black :black
                        :red :red
                        :green :green
                        :yellow :yellow
                        :blue :blue
                        :magenta :magenta
                        :cyan :cyan
                        :white :white})

(def styles
  (for [color (keys colors)
        background (keys background-colors)
        bold [true false]
        italic [true false]
        underline [true false]]
    [(str color background bold italic underline)
     :color (colors color)
     :background (background-colors background)
     :bold bold
     :italic italic
     :underline underline]))

(def server-output-pane
  (s/styled-text :text "Hello world"
                 :wrap-lines? true
                 :preferred-size [640 :by 640]
                 :editable? false
                 :background :black
                 :foreground :white
                 :styles styles))

(def info-pane
  (s/styled-text :text "" 
                 :wrap-lines? true
                 :preferred-size [640 :by 640]
                 :editable? false
                 :background :black
                 :foreground :white
                 :styles styles))

(def display-area
  (s/left-right-split (s/scrollable server-output-pane)
                      (s/scrollable info-pane)
                      :divider-location 1/2))

(def input-textbox
  (s/text :editable? true
          :multi-line? false))

(.setFocusTraversalKeysEnabled input-textbox false)

(def main-panel (s/border-panel :center display-area
                                :south input-textbox))

(def main-window (s/frame :title "mud on the couch"
                          :content main-panel))

(s/listen input-textbox :key-pressed
          (fn [e]
            (let [keycode (.getKeyCode e)
                  prev-key (-> @session-state
                               (:key-press-history)
                               (first))] 
              (condp = keycode
                (KeyEvent/VK_TAB) (tab-complete (s/text input-textbox)
                                                (= keycode prev-key)) 
                (KeyEvent/VK_ENTER) (do (parse-cmd (s/config input-textbox :text))
                                        (s/config! input-textbox :text ""))
                nil)
              (swap! session-state
                     update
                     :key-press-history
                     conj keycode))))


(-> main-window s/pack! s/show!)

(d/show-options server-output-pane)





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




;; Functions
;; =============================================================================


(defn check-alias [cmd]
  ((@client-mods :alias) cmd))


(defn next-cmd []
  (swap! cmd-buffer update :pointer #(if (< % 0) 0 (dec %)))
  (s/config! input-textbox
             :text
             (last (take (@cmd-buffer :pointer)
                         (@cmd-buffer :history)))))


(defn prev-cmd []
  (swap! cmd-buffer update :pointer inc)  
  (s/config! input-textbox
             :text
             (last (take (@cmd-buffer :pointer)
                         (@cmd-buffer :history)))))

(defn put-match-and-update-state
  [pre-key match-set]
  (s/text! input-textbox
           (str pre-key
                " "
                (first match-set)))
  (swap! session-state
         assoc
         :tab-complete-set (rest match-set)))

(defn tab-complete [input-text tab-again?]
  (let [words (clojure.string/split input-text
                                    #" ")
        pre-key (clojure.string/join " "
                                     (butlast words))]
    (if tab-again?
      (let [match-set (@session-state :tab-complete-set)]
        (put-match-and-update-state pre-key match-set))
      (let [search-key (last words) 
            key-pattern (re-pattern (str "\\b"
                                         search-key
                                         "\\w+"))
            match-set (->> (.getText server-output-pane)
                           (re-seq key-pattern)
                           (set)
                           (into '()))]
        (put-match-and-update-state pre-key match-set)))))

(defn update-word [func]
  (let [curr-object (@session-state :curr-object)
        curr-ind (func (@session-state :word-index))]
    (when (and (< curr-ind (count curr-object))
               (>= curr-ind 0))
      (let [curr-target (curr-object curr-ind)]
        (swap! session-state assoc :word-index curr-ind)
        (swap! session-state assoc :selected-target curr-target)
        (>!! render-chan
             #(render-selected-objectbox term-output
                                         5 5 145 5
                                         :magenta :yellow :black
                                         curr-object
                                         curr-ind))
        (>!! render-chan
             #(render-messagebox term-output
                                 102 0 50 5
                                 :red :black :white
                                 (str "Target: "
                                      (@session-state :target-prefix)
                                      "."
                                      curr-target)))))))

(defn prev-word []
  (update-word dec))

(defn next-word []
  (update-word inc))


(defn update-obj [func]
  (if (= func inc)
    (swap! session-state update
           :select-index
           #(if (< % (dec (count (@session-state :objects)))) (func %) %))
    (swap! session-state update
           :select-index
           #(if (> % 0) (func %) %)))
  (swap! session-state assoc
         :curr-object
         (clojure.string/split
          (get-in @session-state
                  [:objects (@session-state :select-index)]) #" "))
  (>!! render-chan
       #(render-objectsbox term-output
                           0 37 102 12
                           :magenta :black :white
                           (@session-state :objects)
                           (@session-state :select-index))))

(defn prev-obj []
  (update-obj dec))

(defn next-obj []
  (update-obj inc))

(defn parse-cmd [cmd]
  (println "Parsing cmd: " cmd)
  (if-let [alias-cmd (check-alias cmd)]
    (>!! cmd-chan alias-cmd)
    (case cmd
      "#quit" (do
                (spit "resources/log.txt" (str @log))
                (reset! running false)
                (close! screen-chan)
                (shutdown-agents))
      "#clear" (s/config! server-output-pane :text "")
      "#select" (swap! session-state assoc :mode :input-key)
      "#cmd-1" (doseq [cmd (get-in @session-state [:binded-cmd :1])] (>!! cmd-chan cmd))
      "#cmd-2" (doseq [cmd (get-in @session-state [:binded-cmd :2])] (>!! cmd-chan cmd))
      "#cmd-3" (doseq [cmd (get-in @session-state [:binded-cmd :3])] (>!! cmd-chan cmd))
      "#cmd-4" (doseq [cmd (get-in @session-state [:binded-cmd :4])] (>!! cmd-chan cmd))
      "#cmd-5" (doseq [cmd (get-in @session-state [:binded-cmd :5])] (>!! cmd-chan cmd))
      "#cmd-1-target" (let [target (str (@session-state :target-prefix)
                                        "."
                                        (@session-state :selected-target))
                            cmds (get-in @session-state [:binded-cmd :1])]
                        (doseq [cmd (butlast cmds)]
                          (>!! cmd-chan cmd))
                        (>!! cmd-chan (str (last cmds) " " target)))
      "#cmd-2-target" (let [target (str (@session-state :target-prefix)
                                        "."
                                        (@session-state :selected-target))
                            cmds (get-in @session-state [:binded-cmd :2])]
                        (doseq [cmd (butlast cmds)]
                          (>!! cmd-chan cmd))
                        (>!! cmd-chan (str (last cmds) " " target)))
      "#cmd-3-target" (let [target (str (@session-state :target-prefix)
                                        "."
                                        (@session-state :selected-target))
                            cmds (get-in @session-state [:binded-cmd :3])]
                        (doseq [cmd (butlast cmds)]
                          (>!! cmd-chan cmd))
                        (>!! cmd-chan (str (last cmds) " " target)))
      "#cmd-4-target" (let [target (str (@session-state :target-prefix)
                                        "."
                                        (@session-state :selected-target))
                            cmds (get-in @session-state [:binded-cmd :4])]
                        (doseq [cmd (butlast cmds)]
                          (>!! cmd-chan cmd))
                        (>!! cmd-chan (str (last cmds) " " target)))
      "#inc-prefix" (do (swap! session-state update :target-prefix inc)
                        (let [curr-object (@session-state :curr-object)
                              curr-ind (@session-state :word-index)
                              curr-target (curr-object curr-ind)]
                          (>!! render-chan
                               #(render-messagebox term-output
                                                   102 0 50 5
                                                   :red :black :white
                                                   (str "Target: "
                                                        (@session-state :target-prefix)
                                                        "."
                                                        curr-target)))))
      "#dec-prefix" (do (swap! session-state update :target-prefix dec)
                        (let [curr-object (@session-state :curr-object)
                              curr-ind (@session-state :word-index)
                              curr-target (curr-object curr-ind)]
                          (>!! render-chan
                               #(render-messagebox term-output
                                                   102 0 50 5
                                                   :red :black :white
                                                   (str "Target: "
                                                        (@session-state :target-prefix)
                                                        "."
                                                        curr-target)))))
      "#load" (reset! client-mods (eval
                                   (read-string
                                    (slurp "resources/slurp.edn"))))
      (>!! cmd-chan cmd))))



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


(defn print-to-server-output-pane []
  (while @running 
    (let [recv-str (<!! screen-chan)
          decolored-str (remove-ansi-escape-code recv-str)]
      (-> recv-str
          (generate-markup)
          (parse-markup (.getDocument server-output-pane)))
      (s/scroll! server-output-pane :to :bottom)
      (>!! trigger-chan decolored-str))))


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
                          (assoc :pointer 0)))))


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


(defn handle-gamepad-cmd []
  (while @running
    (let [cmd (<!! gamepad-chan)]
      (case cmd
        :right-y-down (next-obj)
        :right-y-up (prev-obj)
        :right-x-right (next-word)
        :right-x-left (prev-word)
        :A (parse-cmd (get-in @session-state [:binded-gamepad-cmds :A]))
        :B (parse-cmd (get-in @session-state [:binded-gamepad-cmds :B]))
        :X (parse-cmd (get-in @session-state [:binded-gamepad-cmds :X]))
        :Y (parse-cmd (get-in @session-state [:binded-gamepad-cmds :Y]))
        :rxA (parse-cmd (get-in @session-state [:binded-gamepad-cmds :rxA]))
        :rxB (parse-cmd (get-in @session-state [:binded-gamepad-cmds :rxB]))
        :rxX (parse-cmd (get-in @session-state [:binded-gamepad-cmds :rxX]))
        :rxY (parse-cmd (get-in @session-state [:binded-gamepad-cmds :rxY]))
        :left-bumper (parse-cmd (get-in @session-state [:binded-gamepad-cmds :left-bumper]))
        :right-bumper (parse-cmd (get-in @session-state [:binded-gamepad-cmds :right-bumper]))
        :back nil
        :start nil
        :xbox-guide nil
        :left-thumb (parse-cmd "l")
        :right-thumb (parse-cmd "flee")
        :NW (parse-cmd "nw")
        :N (parse-cmd "n")
        :NE (parse-cmd "ne")
        :E (parse-cmd "e")
        :SE (parse-cmd "se")
        :S (parse-cmd "s")
        :SW (parse-cmd "sw")
        :W (parse-cmd "w")
        :U (parse-cmd "u")
        :D (parse-cmd "d")
        nil)))
  (prn "Stopping handle-gamepad-cmd"))  


;; Start threads
;; =============================================================================

(defn -main
  [& args]
  (prn "Mud starting..............")
  (future (server-output-receiver))
  (future (cmd-sender))
  (future (handle-triggers))
  (future (gamepad-jinput))
  (future (handle-gamepad-cmd))
  (future (print-to-server-output-pane)))

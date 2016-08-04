(ns telnetclient.core
  (:require [lanterna.terminal :as t]
            [clojure.core.async :refer [<! >! <!! >!! chan go close!]])
  (:import [org.apache.commons.net.telnet
            TelnetClient
            TerminalTypeOptionHandler
            EchoOptionHandler
            SuppressGAOptionHandler])
  (:gen-class))

(declare parse-cmd cmd-chan term-output)

;; App states
;; ================================================================================
(def running (atom true))
(def key-buffer (atom ""))
(def cmd-buffer (atom {:history '()
                       :pointer 1}))
(def in-buf (byte-array 1024))
(def screen-buffer (atom []))
(def log (atom []))
(def message-buffer (atom "No new messages"))
(def session-info (atom {:hp "unknown"
                         :max-hp "unknown"
                         :mv "unknown"
                         :max-mv "unknown"
                         :pos "unknown"
                         :exits []}))
(def client-mods (atom (eval (read-string (slurp "resources/slurp.edn")))))

;; Async channel
;; ================================================================================
(def screen-chan (chan))
(def key-stroke-chan (chan))
(def cmd-chan (chan))
(def trigger-chan (chan))


;; Client config
;; ================================================================================
(def tc
  (doto (TelnetClient.)
    (.addOptionHandler (TerminalTypeOptionHandler. "VT100" false false true false))
    (.addOptionHandler (EchoOptionHandler. true false true false))
    (.addOptionHandler (SuppressGAOptionHandler. true true true true))
    ))

(.connect tc "mud.durismud.com" 7777)




;; Streams
;; ================================================================================
(def ips (.getInputStream tc))
(def ops (.getOutputStream tc))




;; Terminals
;; ================================================================================
(def term-input (t/get-terminal
                 :swing
                 {:cols 80
                  :rows 1
                  :palette :gnome}))

;; (def term-output (t/get-terminal
;;                   :unix))
(def term-output (t/get-terminal
                  :unix
                  {:palette :gnome}))




;; Functions
;; ================================================================================


(defn check-alias []
  ((@client-mods :alias) @key-buffer))


(defn next-cmd []
  (swap! cmd-buffer update :pointer #(if (< % 0) 0 (dec %)))
  (reset! key-buffer (last (take (@cmd-buffer :pointer) (@cmd-buffer :history))))
  (t/clear term-input)
  (t/put-string term-input @key-buffer 0 0))


(defn prev-cmd []
  (swap! cmd-buffer update :pointer inc)
  (reset! key-buffer (last (take (@cmd-buffer :pointer) (@cmd-buffer :history))))  
  (t/clear term-input)
  (t/put-string term-input @key-buffer 0 0))

(defn handle-key-stroked [c]
  (swap! key-buffer str c)
  (t/clear term-input)
  (t/put-string term-input @key-buffer 0 0))

(defn key-stroke-capturer []
  (t/in-terminal
   term-input
   (while @running
     (let [c (t/get-key-blocking term-input)]
       (case c
         :escape nil
         :backspace (do (swap! key-buffer (comp (partial apply str) butlast))
                        (t/clear term-input)
                        (t/put-string term-input @key-buffer 0 0))
         :left nil
         :right nil
         :up (prev-cmd)
         :down (next-cmd)
         :insert nil
         :delete nil
         :home nil
         :end nil
         :page-up nil
         :page-down nil
         :tab nil
         :reverse-tab nil
         :enter (do (t/clear term-input)
                    (parse-cmd))
         \space (if-let [alias-cmd (check-alias)]
                  (do (reset! key-buffer (str alias-cmd \space))
                      (t/clear term-input)
                      (t/put-string term-input (str alias-cmd \space) 0 0))
                  (handle-key-stroked \space))
         (handle-key-stroked c))))
   (println "Stopping key-stroke-capturer")))



(defn parse-cmd []
  (if-let [alias-cmd (check-alias)]
    (>!! cmd-chan alias-cmd)
    (let [cmd-txt @key-buffer]
      (case cmd-txt
        "#quit" (do
                  (spit "resources/log.txt" (str @log))
                  (reset! running false)
                  (close! screen-chan)
                  (shutdown-agents))
        "#load" (reset! client-mods (eval (read-string (slurp "resources/slurp.edn"))))
        (>!! cmd-chan cmd-txt)))))



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
  (println "Stopping print-server-output"))

(defn render-messagebox [x y]
  (t/set-bg-color term-output :red)
  (t/put-string term-output (apply str (repeat 50 \space)) x y)
  (t/put-string term-output (apply str (repeat 50 \space)) x (+ y 4))  
  (t/set-bg-color term-output :white)
  (t/set-fg-color term-output :black)  
  (t/put-string term-output (apply str (repeat 50 \space)) x (+ y 1))
  (t/put-string term-output (apply str (repeat 50 \space)) x (+ y 2))
  (t/put-string term-output (apply str (repeat 50 \space)) x (+ y 3))
  (t/put-string term-output @message-buffer (+ x 2) (+ y 2))
  (t/set-bg-color term-output :default)
  (t/set-fg-color term-output :default))

(defn render-statusbox [x y]
  (t/set-bg-color term-output :green)
  (t/put-string term-output (apply str (repeat 50 \space)) x y)
  (t/put-string term-output (apply str (repeat 50 \space)) x (+ y 11))  
  (t/set-bg-color term-output :white)
  (t/set-fg-color term-output :black)  
  (doseq [xx (range 50)
          yy (range 10)]
    (t/put-string term-output " " (+ x xx) (+ y yy 1)))
  (let [{:keys [hp max-hp mv max-mv pos]} @session-info]
    (t/put-string term-output (str "HP: " hp " / " max-hp) (+ x 2) (+ y 2))
    (t/put-string term-output (str "MOVES: " mv " / " max-mv) (+ x 2) (+ y 4))
    (t/put-string term-output (str "POS: " pos) (+ x 2) (+ y 6)))
  (t/set-bg-color term-output :default)
  (t/set-fg-color term-output :default))

(defn render-border [x y w h]
  (t/set-bg-color term-output :blue)
  (t/put-string term-output (apply str (repeat (+ w 2) \-)) x y)
  (t/put-string term-output (apply str (repeat (+ w 2) \-)) x (+ y h 1))
  (doseq [i (range h)]
    (t/put-string term-output "|" x (+ y i 1))
    (t/put-string term-output "|" (+ x w 1) (+ y i 1)))
  (t/set-bg-color term-output :default))

(defn render-exitsbox [x y]
  (t/set-bg-color term-output :red)  
  (t/put-string term-output (apply str (repeat 35 \space)) x y)
  (t/put-string term-output (apply str (repeat 35 \space)) x (+ y 10))  
  (t/set-bg-color term-output :white)
  (t/set-fg-color term-output :black)  
  (doseq [i (range 1 10)]
    (t/put-string term-output (apply str (repeat 35 \space)) x (+ y i)))
  (t/put-string term-output "@" (+ x 12) (+ y 5))
  (t/put-string term-output "-----" (+ x 28) (+ y 5))
  (dorun (map #(case %
                 "Northwest" (t/put-string term-output % (+ x 2) (+ y 2))
                 "North" (t/put-string term-output % (+ x 10) (+ y 2))
                 "Northeast" (t/put-string term-output % (+ x 18) (+ y 2))
                 "West" (t/put-string term-output % (+ x 2) (+ y 5))
                 "East" (t/put-string term-output % (+ x 18) (+ y 5))
                 "Southwest" (t/put-string term-output % (+ x 2) (+ y 8))
                 "South" (t/put-string term-output % (+ x 10) (+ y 8))
                 "Southeast" (t/put-string term-output % (+ x 18) (+ y 8))
                 "Up" (t/put-string term-output % (+ x 30) (+ y 3))
                 "Down" (t/put-string term-output % (+ x 28) (+ y 7))
                 ) (@session-info :exits)))
  (t/set-bg-color term-output :default)
  (t/set-fg-color term-output :default))


(defn print-to-screen [buf]
  (t/clear term-output)
  (dorun (map-indexed
          (fn [line string]
            (t/put-string term-output string 1 (inc line))) buf))
  (render-border 0 0 100 40)
  (render-messagebox 102 0)
  (render-statusbox 102 5)
  (render-exitsbox 102 17))


(defn handle-screen-buffer [lines]
  (swap! screen-buffer #(vec (take-last 5000 (apply conj %1 %2))) lines))

(defn server-output-printer []
  (t/in-terminal
   term-output
   (while @running
     (let [recv-str (<!! screen-chan)
           recv-str-split (clojure.string/split-lines recv-str)
           decolored-recv-str (clojure.string/replace recv-str #"\u001b\[[^m]+m" "")]
       (handle-screen-buffer recv-str-split)
       (>!! trigger-chan decolored-recv-str)
       (print-to-screen (take-last 40 @screen-buffer)))))
  (println "Stopping consume-recv-str"))


(defn re-to-act [buf]
  (fn [[re act]]
    (dorun (map act (re-seq re buf)))))


(defn handle-triggers []
  (while @running
    (let [triggers (@client-mods :trigger)
          buf (<!! trigger-chan)]
      (dorun
       (map (re-to-act buf) triggers))))
  (println "Stopping trigger handlers"))


(defn handle-cmd-history [cmd]
  (swap! cmd-buffer (fn [m]
                      (-> m
                          (update :history conj cmd)
                          (assoc :pointer 1))))
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
  (println "Stopping cmd-sender"))




;; Start threads
;; ================================================================================
(future (key-stroke-capturer))
(future (server-output-receiver))
(future (server-output-printer))
(future (cmd-sender))
(future (handle-triggers))


(defn -main
  [& args]
  (println "Mud starting.............."))

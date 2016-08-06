(ns telnetclient.core
  (:require [lanterna.terminal :as t]
            [telnetclient.ansicode :refer [split-tag-ansi-codes translate]]
            [telnetclient.draw :refer [draw-rect draw-border]]
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

(def term-output (t/get-terminal
                  ;; :unix
                  :swing
                  {
                   :cols 160
                   :rows 50
                   :palette :mac-os-x}))




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
   (prn "Stopping key-stroke-capturer")))



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
        "#clear" (t/clear term-output)
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
  (prn "Stopping print-server-output"))

(defn render-messagebox [scr x y w h borderbg fillbg fillfg]
  (draw-border scr x y w h \. :default borderbg)
  (draw-rect scr (inc x) (inc y) (- w 2) (- h 2) \space :default fillbg)
  (t/set-bg-color scr fillbg)
  (t/set-fg-color scr fillfg)
  (t/put-string scr @message-buffer (+ x 2) (+ y 2))
  (t/set-bg-color scr :default)
  (t/set-fg-color scr :default))

(defn render-statusbox [scr x y w h borderbg fillbg fillfg]
  (draw-border scr x y w h \. :default borderbg)
  (draw-rect scr (inc x) (inc y) (- w 2) (- h 2) \space :default fillbg)
  (t/set-bg-color scr fillbg)
  (t/set-fg-color scr fillfg)  
  (let [{:keys [hp max-hp mv max-mv pos]} @session-info]
    (t/put-string scr (str "HP: " hp " / " max-hp) (+ x 2) (+ y 2))
    (t/put-string scr (str "MOVES: " mv " / " max-mv) (+ x 2) (+ y 4))
    (t/put-string scr (str "POS: " pos) (+ x 2) (+ y 6)))
  (t/set-bg-color scr :default)
  (t/set-fg-color scr :default))

(defn put-coded-text [tags line]
  (t/move-cursor term-output 1 line)
  (doseq [tag tags]
    (case (tag :type)
      :code (doseq [action (translate (tag :val))]
              (case (action :set)
                :default (do (t/set-fg-color term-output :default)
                             (t/set-bg-color term-output :default))
                :style nil
                :fg-bg-color (do (t/set-fg-color term-output (action :val1))
                                 (t/set-bg-color term-output (action :val2)))
                :fg-color (t/set-fg-color term-output (action :val))
                :bg-color (t/set-bg-color term-output (action :val))))
      :text (t/put-string term-output (tag :val)))))


(defn split-and-tag [buf]
  (split-tag-ansi-codes (str " " buf " ")))


(defn trunc-text-96 [buf]
  (loop [tags buf
         space-left 96
         curr-line []
         packed-lines []]
    (let [tag (first tags)]
      (if tag
        (case (tag :type)
          :code (recur (rest tags)
                       space-left
                       (conj curr-line tag)
                       packed-lines)
          :text (let [char-count (count (tag :val))
                      spill-over-count (- char-count space-left)
                      next-tags (rest tags)]
                  (if (> spill-over-count 0)
                    (recur (conj next-tags
                                 (update tag
                                         :val
                                         #((comp (partial apply str " ") drop) %2 %1)
                                         space-left))
                           96
                           []
                           (conj packed-lines
                                 (conj curr-line
                                       (update tag
                                               :val
                                               #((comp (partial apply str) take) %2 %1)
                                               space-left))))
                    (recur next-tags
                           (- space-left char-count)
                           (conj curr-line tag)
                           packed-lines))))
        (conj packed-lines curr-line)))))


(defn prep-buf [buf]
  (->> buf
       (map split-and-tag)
       (map trunc-text-96) 
       (apply concat) 
       (vec)
       (take-last 40)))

(defn render-rawdatabox [scr x y w h borderbg buf]
  (draw-border scr x y w h \. :default borderbg)
  (draw-rect scr (inc x) (inc y) (- w 2) (- h 2) \space :default :default)
  ;; (pack-buffer-80-wide (extract-code-from-text buf))
  (dorun (map-indexed
          (fn [line text]
            ;; (t/put-string term-output text 1 (inc line))
            (put-coded-text text (inc line))
            )
          (prep-buf buf))))

(defn render-exitsbox [scr x y w h borderbg fillbg fillfg]
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
              (@session-info :exits)))
  (t/set-bg-color scr :default)
  (t/set-fg-color scr :default))


(defn print-to-screen [buf]
  (render-rawdatabox term-output 0 0 100 42 :blue buf)
  (render-messagebox term-output 100 0 50 5 :red :white :black)
  (render-statusbox term-output 100 5 50 9 :green :white :black)
  (render-exitsbox term-output 100 14 31 11 :cyan :white :black))


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
;; ================================================================================
(future (key-stroke-capturer))
(future (server-output-receiver))
(future (server-output-printer))
(future (cmd-sender))
(future (handle-triggers))


(defn -main
  [& args]
  (prn "Mud starting.............."))

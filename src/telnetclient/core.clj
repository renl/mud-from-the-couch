(ns telnetclient.core
  (:require [lanterna.terminal :as t]
            [telnetclient.guirender :refer [render-messagebox
                                            render-statusbox
                                            render-rawdatabox
                                            render-exitsbox
                                            render-objectsbox]]
            [clojure.core.async :refer [<! >! <!! >!! chan go close!]])
  (:import [org.apache.commons.net.telnet
            TelnetClient
            TerminalTypeOptionHandler
            EchoOptionHandler
            SuppressGAOptionHandler])
  (:gen-class))

(declare parse-cmd cmd-chan term-output)

;; App states
;; =============================================================================
(def running (atom true))
(def key-buffer (atom ""))
(def cmd-buffer (atom {:history '()
                       :pointer 1}))
(def in-buf (byte-array 1024))
(def screen-buffer (atom []))
(def tab-complete-list (atom #{}))
(def key-stroke-buffer (atom '()))
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
;; =============================================================================
(def screen-chan (chan))
(def key-stroke-chan (chan))
(def cmd-chan (chan))
(def trigger-chan (chan))


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


(defn check-alias []
  ((@client-mods :alias) @key-buffer))


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

(defn key-stroke-capturer []
  (t/in-terminal
   term-input
   (while @running
     (let [c (t/get-key-blocking term-input)]
       (swap! key-stroke-buffer conj c)
       (case c
         :escape nil
         :backspace (do (swap! key-buffer
                               (comp (partial apply str) butlast))
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
        "#load" (reset! client-mods (eval
                                     (read-string
                                      (slurp "resources/slurp.edn"))))
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


(defn print-to-screen [buf]
  (render-rawdatabox term-output 0 0 102 37 :blue buf)
  (render-messagebox term-output 102 0 50 5 :red :white :black @message-buffer)
  (render-statusbox term-output 102 5 50 9 :green :white :black @session-info)
  (render-exitsbox term-output 102 14 31 11 :cyan :white :black @session-info)
  (render-objectsbox term-output 0 37 102 12 :magenta :white :black (@session-info :objects)))


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
(future (key-stroke-capturer))
(future (server-output-receiver))
(future (server-output-printer))
(future (cmd-sender))
(future (handle-triggers))


(defn -main
  [& args]
  (prn "Mud starting.............."))

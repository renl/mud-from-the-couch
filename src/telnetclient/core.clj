(ns telnetclient.core
  (:require [lanterna.terminal :as t]
            [clojure.core.async :refer [<! >! <!! >!! chan go close!]])
  (:import [org.apache.commons.net.telnet TelnetClient TerminalTypeOptionHandler EchoOptionHandler SuppressGAOptionHandler])
  (:gen-class))

(declare parse-cmd cmd-chan term-output)

;; App states
;; ================================================================================
(def running (atom true))
(def key-buffer (atom []))
(def cmd-buffer (atom []))
(def in-buf (byte-array 1024))
(def screen-buffer (atom ""))
(def client-mods (atom (eval (read-string (slurp "resources/slurp.edn")))))
(def log (atom []))

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
                  :text
                  {:palette :gnome}))







;; Functions
;; ================================================================================

(defn check-alias []
  ((@client-mods :alias) (apply str @key-buffer)))

(defn prev-cmd []
  (reset! key-buffer (peek @cmd-buffer))
  (t/clear term-input)
  (t/put-string term-input (apply str @key-buffer))
  (swap! cmd-buffer pop))

(defn handle-key-stroked [c]
  (swap! key-buffer conj c)
  (t/put-character term-input c))

(defn key-stroke-capturer []
  (t/in-terminal
   term-input
   (while @running
     (let [c (t/get-key-blocking term-input)]
       (case c
         :enter (do (t/clear term-input) (parse-cmd))
         :up (prev-cmd)
         \space (if-let [alias-cmd (check-alias)]
                  (do (reset! key-buffer (conj (vec alias-cmd) \space))
                      (t/clear term-input)
                      (t/put-string term-input (str alias-cmd \space)))
                  (handle-key-stroked \space))
         :backspace (do (swap! key-buffer #(if (empty? %) % (pop %)))
                        (t/clear term-input)
                        (t/put-string term-input (apply str @key-buffer)))
         (handle-key-stroked c))))
   (println "Stopping key-stroke-capturer")))







(defn parse-cmd []
  (if-let [alias-cmd (check-alias)]
    (>!! cmd-chan alias-cmd)
    (let [cmd-txt (apply str @key-buffer)]
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



(defn server-output-printer []
  (t/in-terminal
   term-output
   (while @running
     (let [recv-str (<!! screen-chan)
           decolored-recv-str (clojure.string/replace recv-str #"\u001b\[[^m]+m" "")]
       (t/put-string term-output recv-str)
       (swap! screen-buffer str decolored-recv-str)
       (>!! trigger-chan decolored-recv-str))))
  (println "Stopping consume-recv-str"))


(defn handle-triggers []
  (while @running
    (let [triggers (@client-mods :trigger)
          buf (<!! trigger-chan)]
      (doall (for [[regex action] triggers]
               (doall (for [[_ & grps] (re-seq regex buf)]
                        (action grps))))))))




(defn cmd-sender []
  (while @running
    (let [cmd (<!! cmd-chan)
          cmd-vec (vec (str cmd \newline))
          out-buf (byte-array (map (comp byte int) cmd-vec))
          i (count cmd-vec)]
      (.write ops out-buf 0 i)
      (.flush ops)
      (swap! cmd-buffer conj cmd)
      (reset! key-buffer [])))
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

(ns telnetclient.core
  (:require [lanterna.terminal :as t])
  (:import [org.apache.commons.net.telnet TelnetClient TerminalTypeOptionHandler EchoOptionHandler SuppressGAOptionHandler])
  (:gen-class))

(def running (atom true))

(def tc
  (doto (TelnetClient.)
    (.addOptionHandler (TerminalTypeOptionHandler. "VT100" false false true false))
    (.addOptionHandler (EchoOptionHandler. true false true false))
    (.addOptionHandler (SuppressGAOptionHandler. true true true true))
    ))

(.connect tc "mud.durismud.com" 7777)

(def ips (.getInputStream tc))
(def ops (.getOutputStream tc))
(def in-buf (byte-array 1024))

(def terminal-size (ref [0 0]))

(defn handle-resize [cols rows]
  (dosync (ref-set terminal-size [cols rows])))

(def term-input (t/get-terminal
           :swing
           {:resize-listener handle-resize
            :cols 80
            :rows 2}))

(def term-output (t/get-terminal
                  :unix))

(def key-buffer (atom []))
(def cmd-buffer (atom []))

(defn send-cmd []
  (let [out-buf (byte-array (map (comp byte int) @key-buffer))
        i (count @key-buffer)]
    (.write ops out-buf 0 i)
    (.flush ops)
    (t/clear term-input)
    (swap! cmd-buffer conj @key-buffer)
    (reset! key-buffer [])))

(defn parse-cmd []
  (cond
    (= "quit" (apply str @key-buffer)) (do (reset! running false) (shutdown-agents))
    :else (do
            (swap! key-buffer conj \newline)
            (future (send-cmd)))))

(defn prev-cmd []
  (reset! key-buffer (pop (peek @cmd-buffer)))
  (t/clear term-input)
  (t/put-string term-input (apply str @key-buffer))
  (swap! cmd-buffer pop))

(defn receive-cmd []
  (t/in-terminal
   term-input
   (while @running
     (let [c (t/get-key-blocking term-input)]
       (case c
         :enter (parse-cmd)
         :up (prev-cmd)
         :backspace (do (swap! key-buffer pop)
                        (t/clear term-input)
                        (t/put-string
                         term-input
                         (apply str @key-buffer)))
         (do (swap! key-buffer conj c)
             (t/put-character term-input c)))))
   (println "Stopping receive-cmd")))

(defn print-server-output []
  (t/in-terminal
   term-output
   (while @running
     (let [j (.available ips)]
       (if (> j 1024)
         (let [i (.read ips in-buf 0 1024)]
           (t/put-string term-output (String. in-buf 0 i)))
         (let [i (.read ips in-buf 0 j)]
           (t/put-string term-output (String. in-buf 0 i))))))
   (println "Stopping print-server-output")))

(future (receive-cmd))
(future (print-server-output))

(defn -main
  [& args]
  (println "Mud starting.............."))

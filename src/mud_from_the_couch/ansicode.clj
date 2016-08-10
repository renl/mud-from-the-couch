(ns mud-from-the-couch.ansicode)

(defn split-tag-ansi-codes [text]
  (let [code (map (fn [match] {:type :code :val match})
                  (re-seq #"\u001b\[[^m]+m" text))
        val (map (fn [match] {:type :text :val match})
                 (clojure.string/split text #"\u001b\[[^m]+m"))]
    (conj (vec (interleave val code)) (last val))))

(defn translate [code]
  (let [vals (re-seq #"(\d+)[;m]" code)]
    (map #(case (read-string (last %))
            0 {:set :default :val :default}
            ;; 1 {:set :default :val :default}
            ;; 4 {:set :default :val :default}
            ;; 5 {:set :default :val :default}
            ;; 7 {:set :default :val :default}
            1 {:set :style :val :bold}
            4 {:set :style :val :underline}
            5 {:set :style :val :blinking}
            7 {:set :style :val :reverse}
            8 {:set :default :val :default}
            30 {:set :fg-color :val :green}
            ;; 30 {:set :fg-bg-color :val1 :black :val2 :white}            
            31 {:set :fg-color :val :red}
            32 {:set :fg-color :val :green}
            33 {:set :fg-color :val :yellow}
            34 {:set :fg-color :val :blue}
            35 {:set :fg-color :val :magenta}
            36 {:set :fg-color :val :cyan}
            37 {:set :fg-color :val :white}
            40 {:set :bg-color :val :black}
            41 {:set :bg-color :val :red}
            42 {:set :bg-color :val :green}
            43 {:set :bg-color :val :yellow}
            44 {:set :bg-color :val :blue}
            45 {:set :bg-color :val :magenta}
            46 {:set :bg-color :val :cyan}
            47 {:set :bg-color :val :white}
            {:set :default :val :default}) vals)))

(ns mud-from-the-couch.ansicode)

(defn remove-ansi-escape-code [text]
  (clojure.string/replace text #"\u001b\[[^m]+m" ""))

(defn generate-markup [buffer]
  (let [esc-code (map (fn [match] {:type :code :val match})
                  (re-seq #"\u001b\[[^m]+m" buffer))
        esc-code# (count esc-code)
        text (map (fn [match] {:type :text :val match})
                 (clojure.string/split buffer #"\u001b\[[^m]+m"))
        text# (count text)]
    (cond
      (nil? esc-code) [(first text)]
      (> text# esc-code#) (conj (vec (interleave text
                                                 esc-code))
                                (last text))
      (= text# esc-code#) (vec (interleave text esc-code))
      (< text# esc-code#) [(first esc-code)])))

(defn handle-code [style code]
  (case code
    0 {:color :white
       :background :black
       :bold false
       :italic false
       :underline false} 
    1 (assoc style :bold true)
    4 (assoc style :underline true)
    5 (assoc style :italic true)
    7 style
    8 style
    30 (assoc style :color :black) 
    31 (assoc style :color :red)
    32 (assoc style :color :green)
    33 (assoc style :color :yellow)
    34 (assoc style :color :blue)
    35 (assoc style :color :magenta)
    36 (assoc style :color :cyan)
    37 (assoc style :color :white)
    40 (assoc style :background :black)
    41 (assoc style :background :red)
    42 (assoc style :background :green)
    43 (assoc style :background :yellow)
    44 (assoc style :background :blue)
    45 (assoc style :background :magenta)
    46 (assoc style :background :cyan)
    47 (assoc style :background :white)
    style))

(defn parse-markup [markup doc] 
  (reduce (fn [style tag]
            (case (tag :type)
              :code (let [codes (map (comp read-string
                                           last)
                                     (re-seq #"(\d+)[;m]"
                                             (tag :val)))]
                      (reduce (fn [curr-style code]
                                (handle-code curr-style code))
                              style
                              codes))
              :text (do (.insertString doc
                                       (.getLength doc)
                                       (tag :val)
                                       (.getStyle doc
                                                  (str (style :color)
                                                       (style :background)
                                                       (style :bold)
                                                       (style :italic)
                                                       (style :underline))))
                        style)))
          {:color :white
           :background :black
           :bold false
           :italic false
           :underline false}
          markup))

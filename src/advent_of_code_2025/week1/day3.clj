(ns advent-of-code-2025.week1.day3
  (:require [clojure.pprint :as io]))

(def example (slurp "./resources/y2025/day3/example.txt"))
(def input (slurp "./resources/y2025/day3/input.txt"))

(defn char-seq-to-ints
  [char-seq]
  (map #(Integer/parseInt (str %)) char-seq))

(defn parse-to-input
  [raw-input]
  (let [splitted-lines (clojure.string/split-lines raw-input)
        converted-lines (map #(char-seq-to-ints %) splitted-lines)
        ]
    converted-lines))

(parse-to-input example)

(defn largest-in-length-n
  [coll n]
  (let [max-index (- (count coll) n)
        ;_ (println "coll: " coll)
        ]
    (loop [largest 0
           largest-index 0
           index 0]
      (if (> index max-index)
        {:largest largest, :largest-index largest-index}
        (let [curr (get coll index)
              curr-is-larger (> curr largest)
              new-largest (if (true? curr-is-larger) curr largest)
              new-largest-index (if (true? curr-is-larger) index largest-index)
              ]
          (recur new-largest new-largest-index (inc index))))
      )))

(defn largest-in-length-n-recur
  [coll n]
  (loop [acc []
         sub-coll coll
         count n]
    (if (zero? count)
      acc
      (let [info (largest-in-length-n sub-coll count)
            ;;_ (println info count)
            new-sub-coll (subvec sub-coll  (inc (:largest-index info)))]
        (recur (conj acc (:largest info)) new-sub-coll (dec count)))
      )))

(defn solve-puzzle
  [input n]
  (let [converted-lines (parse-to-input input)
        vector-lines (map #(vec %) converted-lines)
        highest-jolts (map #(largest-in-length-n-recur % n) vector-lines)
        highest-jolts-longs (map (fn [coll] (Long/parseLong (apply str coll))) highest-jolts)
        ;;        _ (println highest-jolts-longs)
        ]
    (apply + highest-jolts-longs))
  )


;; 17376
(solve-puzzle input 2)
(solve-puzzle input 12)

(ns advent-of-code-2024.week1.day1
  (:require [advent-of-code-2024.utils.io :as io]
            [clojure.string :as str]))

(defn convert-line
  [line]
  (map io/str->int
    (filter (complement str/blank?) (str/split line #" "))))

(defn solve-part-1
  "docstring"
  [filename]
  (let [collection (map convert-line  (io/read-input filename))
        leftCollection (map #(nth % 0) collection)
        rightCollection (map #(nth % 1) collection)
        ]
    (->>
      (map vector (sort leftCollection) (sort rightCollection))
      (map #(reduce - %))
      (map #(abs %))
      (io/sum)
      )))


(defn solve-part-2
  "docstring"
  [filename]
  (let [collection (map convert-line  (io/read-input filename))
        leftCollection (map #(nth % 0) collection)
        rightCollection (map #(nth % 1) collection)
        rightCollectionFrequencies (frequencies rightCollection)
        ]
    (->>
      (map #(get rightCollectionFrequencies %1) leftCollection)
      (map vector leftCollection)
      (filter #(some? (nth % 1)))
      (map #(reduce * %))
      (io/sum)
      )))
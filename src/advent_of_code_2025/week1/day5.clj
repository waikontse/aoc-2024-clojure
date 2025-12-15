(ns advent-of-code-2025.week1.day5
  (:require [advent-of-code-2024.utils.io :as io]))

(def example (slurp "./resources/y2025/day5/example.txt"))
(def input (slurp "./resources/y2025/day5/input.txt"))

(defn convert-raw-range
  "convert e.g. 3-5 to {:start 3 :end 5}"
  [raw-range]
  (let [splitted (clojure.string/split raw-range #"-")
        start (io/str->int (get splitted 0))
        end (io/str->int (get splitted 1))]
    {:start start :end end})
  )

(defn parse-input-to-puzzle
  [raw-input]
  (let [splitted (clojure.string/split-lines raw-input)
        index-of-split (.indexOf splitted "")
        _ (println index-of-split)
        converted-ranges (->> (subvec splitted 0 index-of-split)
                              (map #(convert-raw-range %))
                              (sort-by :start)
                              )
        ingredients (subvec splitted (inc index-of-split))
        ]
    {:ranges converted-ranges :ingredients ingredients})
  )

(defn is-fresh-ingredient?
  [fresh-range ingredient-id]
  (and (>= ingredient-id (:start fresh-range))
       (<= ingredient-id (:end fresh-range)))
  )

(defn is-fresh-ingredient-recur?
  "docstring"
  [fresh-ranges ingredient-id]
  (loop [fresh-ranges-left fresh-ranges
         range-has-been-found false
         ]
    (cond
      (true? range-has-been-found) range-has-been-found
      (empty? fresh-ranges-left) range-has-been-found
      :else (let [current-range (first fresh-ranges-left)
                  is-fresh? (is-fresh-ingredient? current-range ingredient-id)
                  ]
        (recur (rest fresh-ranges-left) is-fresh?))))
  )

(parse-input-to-puzzle example)

(defn solve
  [input]
  (let [parsed-puzzle (parse-input-to-puzzle input)
        fresh-ingredients (reduce (fn [coll item]
                                    (when (is-fresh-ingredient-recur? (:ranges parsed-puzzle) (:ingredients parsed-puzzle))
                                      (conj coll item))
                                    )
                                  []
                                  (:ingredients parsed-puzzle))]
    fresh-ingredients)
  )

(solve example)
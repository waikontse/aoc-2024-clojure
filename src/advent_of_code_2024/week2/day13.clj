(ns advent-of-code-2024.week2.day13
  (:require [advent-of-code-2024.utils.io :as io]
            [advent-of-code-2024.utils.algorithms :as algo]
            [clojure.pprint :as pp]))

;; Break the 3 lines up
(def not-blank? (complement clojure.string/blank?))
(def xOrY #"(X\+|Y\+|X\=|Y\=)")
(def headers #"(Button A:|Button B:|Prize:)")

(defn clean-up
  [str item]
  (clojure.string/replace item str ""))

(defn split
  [item]
  (clojure.string/split item #", "))

(defn raw-line->ints
  [raw-line]
  (->> (clean-up headers raw-line)
       (clojure.string/trim)
       (split)
       (map #(clean-up xOrY %))
       (map #(io/str->int %))))

(defn raw-lines->ints
  [raw-lines]
  (map #(raw-line->ints %) raw-lines))

(defn ints->expanded-form
  [xs]
  (apply map vector xs))

(defn is-matrix-okay?
  [values pred]
  (every? #(pred %) values))

(defn calc-price
  [values]
  (+ (* 3 (first values)) (second values)))

(def is-type-long? #(= (type %) (type 1)))
(def is-value<100? #(< % 100))
(def pred-part2 #(and (is-type-long? %) (fn [val] (is-value<100? val))))

(defn solve-and-print-all
  [matrixes pred extra]
  (let [all-solved (map #(algo/solve-by-determinant % extra) matrixes)
        filtered (filter #(is-matrix-okay? % pred) all-solved)
        all-coins (map #(calc-price %) filtered)
        _ (println (io/sum all-coins))
        ])
  )

(defn solve-all
  [filename]
  (let [raw-strings (io/read-input "day13/input.txt")
        no-blanks (->>
                    (filter not-blank? raw-strings)
                    (partition 3))
        mapped-lines (map #(raw-lines->ints %) no-blanks)
        all-matrix-form (map #(ints->expanded-form %) mapped-lines)

        _ (solve-and-print-all all-matrix-form is-type-long? 0)
        _ (solve-and-print-all all-matrix-form pred-part2 10000000000000)
        ])
  )

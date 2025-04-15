(ns advent-of-code-2024.week2.day13
  (:require [advent-of-code-2024.utils.io :as io]
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

(defn row-divide-by
  [row divisor]
  (map #(/ % divisor) row))

(defn row-multiply-by
  [row multiplier]
  (map #(* % multiplier) row))

(defn row-add
  [m1 m2]
  (->> (map vector m1 m2)
       (map #(apply + %))))

(defn row-subtract
  [m1 m2]
  (->> (map vector m1 m2)
       (map #(apply - %))))

(defn row-reduce
  [reduced toBeReduced col]
  (let [multiplier (nth toBeReduced col)
        multipliedReduced (row-multiply-by reduced multiplier)]
    (row-subtract toBeReduced multipliedReduced)))

(defn row-simplify-col
  [row col]
  (let [divisor (nth row col)]
    (row-divide-by row divisor)))

(defn matrix-reduce-col
  "docstring"
  [matrix col]
  (let [before (flatten (take col matrix))
        reduced (row-simplify-col (nth matrix col) col)
        rest (drop (inc col) matrix)
        reduced-rest (map #(row-reduce reduced % col) rest)]
    (->> (concat [before reduced] reduced-rest)
         (filter (complement empty?)))))

(defn matrix-reduce-fully
  [matrix]
  (reduce (fn [coll item]
            (matrix-reduce-col coll item))
          (matrix-reduce-col matrix 0)
          (range 1 (count matrix)))
  )

(defn matrix-simplify-col
  [matrix col]
  (let [reducer (nth matrix col)
        before  (take col matrix)
        after (drop (inc col) matrix)
        simplified-before (map #(row-reduce reducer % col) before)
        ]
    (->> (concat [(flatten simplified-before) reducer] after)
         (filter (complement empty?)))))

(defn matrix-simplify-fully
  [matrix]
  (let [length (count matrix)]
    (reduce (fn [coll item]
              (matrix-simplify-col coll item))
            (matrix-simplify-col matrix (dec length))
            (reverse (range 1 (dec length))))
    ))

(defn matrix-to-echelon
  [matrix]
  ;(println "Reducing form to echelon")
  ;(pp/pprint matrix)
  (->> (matrix-reduce-fully matrix)
       (matrix-simplify-fully)))

(def full-matrix '([94 22 8400] [34 67 5400]))
(matrix-reduce-fully full-matrix)
(matrix-to-echelon full-matrix)

(defn calc-price
  [matrix]
  (let [first (last (first matrix))
        second (last (second matrix))]
    (+ (* first 3) second))
  )

(defn is-matrix-okay?
  [matrix]
  (->> (map (fn [row] (every? #(= (type %) (type 1N)) row)) matrix)
       (every? true?)))

(def solved '((1N 0N 80N) (0N 1N 40N)))
(is-matrix-okay? solved)
(calc-price solved)

(defn solve-part-1
  ""
  [filename]
  (let [raw-strings (io/read-input "day13/example.txt")
        no-blanks (->>
                    (filter not-blank? raw-strings)
                    (partition 3))
        _ (run! println no-blanks)
        mapped-lines  (map #(raw-lines->ints %) no-blanks)
        ;;_ (pp/pprint mapped-lines)
        all-matrix-form (map #(ints->expanded-form %) mapped-lines)
        _ (pp/pprint all-matrix-form)
        all-solved (map #(matrix-to-echelon %) all-matrix-form)
        _ (pp/pprint all-solved)
        filtered (filter is-matrix-okay? all-solved)
        all-coins (map #(calc-price %) filtered)
        _ (println (io/sum all-coins))
        ])
  0)

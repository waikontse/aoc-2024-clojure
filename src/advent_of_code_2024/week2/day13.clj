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
        before (take col matrix)
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

;(def full-matrix '([94 22 8400] [34 67 5400]))
;(def full-matrix2 '([5 6 100] [5 6 100]))
;(matrix-reduce-fully full-matrix)
;(matrix-to-echelon full-matrix2)

(defn calc-price
  [matrix]
  (let [first (last (first matrix))
        second (last (second matrix))]
    (+ (* first 3) second))
  )

(def is-long-or-bigInt?
  (fn [val]
    (or (= (type val) (type 1N))
        (= (type val) (type 1)))
    )
  )

(defn is-matrix-okay?
  [matrix]
  (->> (map (fn [row] (every? is-long-or-bigInt? row)) matrix)
       (every? true?)))

(def solved '((1N 0N 80N) (0N 1N 40N)))
(is-matrix-okay? solved)
(calc-price solved)


(defn is-matrix-okay2?
  [values pred]
  (every? #(pred %) values))

(is-matrix-okay2? [1 6] #(= (type %) (type 1)))

(defn determinant
  "docstring"
  [matrix]
  (let [row1 (first matrix)
        row2 (second matrix)
        a (first row1)
        b (second row1)
        c (first row2)
        d (second row2)
        ]
    (- (* a d) (* b c))))

(defn solve-by-determinant
  [matrix extra]
  (let [row1 (first matrix)
        row2 (second matrix)
        vector-col [(+ extra (last row1)) (+ extra (last row2))]
        d (determinant matrix)
        dx (determinant [(assoc row1 0 (first vector-col))
                         (assoc row2 0 (second vector-col))])
        dy (determinant [(assoc row1 1 (first vector-col))
                         (assoc row2 1 (second vector-col))])
        ]
    [(/ dx d) (/ dy d)]))

(defn calc-price2
  [values]
  (+ (* 3 (first values)) (second values)))

(def full-matrix '([94 22 8400] [34 67 5400]))
(def full-matrix2 '([26 67 12748] [66 21 12176]))
(def error-1 '([23 23 1012] [12 59 2079]))
(def error-2 '([46 95 4597] [86 17 1367]))
(solve-by-determinant full-matrix 0)
(matrix-to-echelon full-matrix)
(calc-price (matrix-to-echelon full-matrix))
(matrix-to-echelon error-1)

(calc-price2 [80 40])

(determinant [[1 2] [3 4]])

(def is-type-long? #(= (type %) (type 1)))
(def is-value<100? #(< % 100))
(def pred-part2 #(and (is-type-long? %) (fn [val] (is-value<100? val))))

(defn solve-and-print-all
  [matrixes pred extra]
  (let [all-solved (map #(solve-by-determinant % extra) matrixes)
        filtered (filter #(is-matrix-okay2? % pred) all-solved)
        all-coins (map #(calc-price2 %) filtered)
        _ (println (io/sum all-coins))
        ])
  )

(defn solve-and-print-all-gauss
  [matrixes]
  (let [all-solved (map #(matrix-to-echelon %) matrixes)
        filtered (filter #(is-matrix-okay? %) all-solved)
        all-coins (map #(calc-price %) filtered)
        _ (println (io/sum all-coins))
        ])
  )

(defn solve-part-1-gauss
  [filename]
  (let [raw-strings (io/read-input "day13/input.txt")
        no-blanks (->>
                    (filter not-blank? raw-strings)
                    (partition 3))
        mapped-lines (map #(raw-lines->ints %) no-blanks)
        all-matrix-form (map #(ints->expanded-form %) mapped-lines)

        _ (solve-and-print-all-gauss all-matrix-form)
        ])
  )

(defn solve-part-1
  [filename]
  (let [raw-strings (io/read-input "day13/input.txt")
        no-blanks (->>
                    (filter not-blank? raw-strings)
                    (partition 3))
        mapped-lines (map #(raw-lines->ints %) no-blanks)
        all-matrix-form (map #(ints->expanded-form %) mapped-lines)

        _ (solve-and-print-all all-matrix-form is-type-long? 0)

        ;_ (solve-and-print-all all-matrix-form is-type-long? 0)
        ;_ (solve-and-print-all all-matrix-form pred-part2 10000000000000)
        ])
  )

;; 30973

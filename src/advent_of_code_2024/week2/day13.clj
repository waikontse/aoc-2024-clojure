(ns advent-of-code-2024.week2.day13
  (:require [advent-of-code-2024.utils.io :as io]))

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

(ints->expanded-form '((94 34) (22 67) (8400 5400)))
(ints->expanded-form [[94 34] [22 67] [8400 5400]])

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

(defn row-simplify-col
  [row col]
  (let [divisor (nth row col)]
    (row-divide-by row divisor)))

(defn matrix-reduce-col
  "docstring"
  [matrix col]
  (let [target (row-simplify-col matrix col)]
    )
  )

(row-simplify-col [94 22 8400] 0)
(row-divide-by [94 22 8400] 94)

(def full-matrix '([94 22 8400] [34 67 5400]))

(def mt1 [94 22 8400])
(def mt2 [34 67 5400])

(def int1 (-> (row-simplify-col mt1 0)
              (row-multiply-by (first mt2))
              ))

(-> (row-simplify-col mt1 0)
    (row-multiply-by (first mt2))
    )

(defn invert-ratio
  [ratio]
  (/ (denominator ratio) (numerator ratio)))

(invert-ratio 3455/67)

(denominator 23/45)

(def row2 (row-subtract mt2 int1))
(row-multiply-by row2 (invert-ratio (second row2)))

(raw-line->ints "Button A: X+94, Y+34")
(raw-line->ints "Button B: X+22, Y+67")
(raw-line->ints "Prize: X=8400, Y=5400")


(defn solve-part-1
  ""
  [filename]
  (let [raw-strings (io/read-input "day13/example.txt")
        no-blanks (->>
                    (filter not-blank? raw-strings)
                    (partition 3))
        _ (run! println no-blanks)
        mapped-lines (raw-lines->ints (first no-blanks))
        _ (println mapped-lines)
        _ (println (ints->expanded-form mapped-lines))
        ])
  0)

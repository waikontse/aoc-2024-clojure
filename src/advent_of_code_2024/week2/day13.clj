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
  (let [first-row (map #(first %) xs)
        second-row (map #(second %) xs)
        ]
    [first-row second-row]))

(defn matrix-divide-by
  [matrix divisor]
  (map #(/ % divisor) matrix))

(defn matrix-multiply-by
  [matrix multiplier]
  (map #(* % multiplier) matrix))

(defn matrix-add
  [m1 m2]
  (->> (map vector m1 m2)
       (map #(apply + %))))

(defn matrix-subtract
  [m1 m2]
  (->> (map vector m1 m2)
       (map #(apply - %))))


(defn matrix-simplify-col
  [matrix col]
  (let [divisor (nth matrix col)
        ]
    (matrix-divide-by matrix divisor)))

(matrix-simplify-col [94 22 8400] 0)
(matrix-divide-by [94 22 8400] 94)

(def mt1 [94 22 8400])
(def mt2 [34 67 5400])

(def int1 (-> (matrix-simplify-col mt1 0)
              (matrix-multiply-by (first mt2))
              ))

(-> (matrix-simplify-col mt1 0)
    (matrix-multiply-by (first mt2))
    )

(defn invert-ratio
  [ratio]
  (/ (denominator ratio)(numerator ratio)))

(invert-ratio 3455/67)

(denominator 23/45)

(def row2 (matrix-subtract mt2 int1))
(matrix-multiply-by row2 (invert-ratio (second row2)))






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

(ns advent-of-code-2024.week1.day2
  (:require [advent-of-code-2024.utils.io :as io]
            [clojure.string :as str]))

(def sample-data (slurp "./resources/day2/example.txt"))
(run! println (str/split-lines sample-data))

(defn is-strictly?
  [f my-comp  max numbers]
  (let [less-then-max (map #(my-comp % max) numbers)
        are-strictly-numbers (map f numbers)
        combined (flatten (map vector less-then-max are-strictly-numbers))
        _ (println "Combined:" combined)
        _ (println "Calling is strictly")
        _ (println "numbers:" numbers)
        result (every? identity combined)
        _ (println "Result: " result)
        ]
    result))

(defn calc-diff
  [arglist]
  (- (first arglist) (second arglist)))

(defn solve-part-1
  [filename]
  (let [data (io/read-input "day2/input.txt")
        clean-data (->>
                     data
                     (map #(str/split % #" "))
                     (map #(map io/str->int %))
                     (map #(partition 2 1 %)))
        calc-data (map #(map calc-diff %) clean-data)
        _ (println calc-data)
        is-strictly-positive (count (filter #(is-strictly? pos-int? <= 3 %) calc-data))
        is-strictly-negative (count (filter #(is-strictly? neg-int? >= -3 %) calc-data))
        ]
    (+ is-strictly-positive is-strictly-negative))
  )

(partition 2 1 [1 2 3 4])
(solve-part-1 "")

(defn solve-part-2
  "docstring"
  [filename]
  )


;;(def sample (slurp "resources/day2/example.txt"))

;; Solution from Thomas van der Veen

;(def all (slurp "input.txt"))
;(def small (slurp "small.txt"))

;(let [data (-> all
;               (split-lines)
;               (->>
;                 (map #(split % #"   "))))
;      col-a (sort (mapv #(Integer/parseInt %) (map first data)))
;      col-b (sort (mapv #(Integer/parseInt % ) (mapv second data)))
;      diff (map #(abs (- %1 %2)) col-a col-b )
;      total (reduce + diff)
;      ]
;  total)


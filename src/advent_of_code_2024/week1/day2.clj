(ns advent-of-code-2024.week1.day2
  (:require [advent-of-code-2024.utils.io :as io]
            [clojure.string :as str]
            [clojure.core.reducers :as r]))

(defn replace-val
  [truths]
  (let [position (.indexOf truths false)
        _ (println truths)]
    (if (> position -1)
      (assoc truths position true)
      truths)
    )
  )

(defn is-strictly?
  [f my-comp max numbers compensator]
  (let [less-then-max (map #(my-comp % max) numbers)
        are-strictly-numbers (map f numbers)
        combined (into [] (r/flatten (map vector less-then-max are-strictly-numbers)))
        _ (println numbers)
        _ (println "combined: " combined)
        result (every? identity (compensator combined))
        ]
    ()
    result))

(defn calc-diff
  [arglist]
  (- (first arglist) (second arglist)))

(defn solve-part-1
  [filename]
  (println "solving part 1 with filename: " filename)
  (let [data (io/read-input filename)
        clean-data (->>
                     data
                     (map #(str/split % #" "))
                     (map #(map io/str->int %))
                     (map #(partition 2 1 %)))
        calc-data (map #(map calc-diff %) clean-data)
        is-strictly-positive (count (filter #(is-strictly? pos-int? <= 3 % identity) calc-data))
        is-strictly-negative (count (filter #(is-strictly? neg-int? >= -3 % identity) calc-data))
        ]
    (+ is-strictly-positive is-strictly-negative))
  )

(defn solve-part-2
  [filename]
  (let [data (io/read-input filename)
        clean-data (->>
                     data
                     (map #(str/split % #" "))
                     (map #(map io/str->int %))
                     (map #(partition 2 1 %)))
        calc-data (map #(map calc-diff %) clean-data)
        is-strictly-positive (count (filter #(is-strictly? pos-int? <= 3 % replace-val) calc-data))
        _ (println calc-data)
        _ (println "positive: " is-strictly-positive)
        is-strictly-negative (count (filter #(is-strictly? neg-int? >= -3 % replace-val) calc-data))
        ]
    (+ is-strictly-negative is-strictly-positive)))


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


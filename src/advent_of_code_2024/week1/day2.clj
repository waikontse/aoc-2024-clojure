(ns advent-of-code-2024.week1.day2
  (:require [advent-of-code-2024.utils.io :as io]
            [clojure.core.reducers :as r]
            [clojure.string :as str]))

(defn is-strictly?
  [f my-comp max numbers]
  (let [;_ (println "received numbers :" numbers)
        less-then-max (map #(my-comp % max) numbers)
        are-strictly-numbers (map f numbers)
        combined (into [] (r/flatten (map vector less-then-max are-strictly-numbers)))
        ;_ (println "combined: " combined)
        result (every? identity combined)
        ]
    ()
    result))

(defn calc-diff
  [arglist]
  (- (second arglist) (first arglist)))

(defn generate-rows
  [numbers]
  (println "generating number: " numbers)
  (let [length (count numbers)
        ]
    (->
      (map #(io/vec-remove % numbers) (range length))
      (conj numbers)
      ))
  )

(defn solve-part-1
  [filename]
  (println "solving part 1 with filename: " filename)
  (let [data (io/read-input "day2/example.txt")
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

(defn solve-part-2
  [filename]
  (let [data (io/read-input "day2/example.txt")
        clean-data (->>
                     data
                     (map #(str/split % #" "))
                     (map #(mapv io/str->int %))
                     (map generate-rows)
                     (map #(map (fn [param] (partition 2 1 param)) %))
                     )
        ; calc-data (map #(map calc-diff %) clean-data)
        ;_ (println "printing clean data: " clean-data)
        _ (doseq [list clean-data]
            (doseq [item list]  (println (mapv calc-diff item))))
            ;(for [item list] (reduce println "item: " item)))
        ;_ (reduce println items)
        ;generated-rows (map generate-rows calc-data)
        ;;is-strictly-generated-rows (map #(map (fn [numbers] (is-strictly? pos-int? <= 3 numbers)) %) generated-rows)
        ;a (map #(map (fn [numbers] (is-strictly? pos-int? <= 3 numbers)) %) generated-rows)
        ;b (map #(map (fn [numbers] (is-strictly? neg-int? >= 3 numbers)) %) generated-rows)
        ;_ (reduce println generated-rows)
        ;_ (reduce println a)
        ;_ (reduce println b)
        ;_ (println "positive: " is-strictly-positive)
        ;is-strictly-positive (count (filter #(is-strictly? pos-int? <= 3 %) calc-data))
        ;is-strictly-negative (count (filter #(is-strictly? neg-int? >= -3 %) calc-data))
        ]
    (+ 1 2))
  ;(+ is-strictly-negative is-strictly-positive))
  )



; ([-1 -2 -2 -1] [-2 -2 -1] [-1 -2 -1] [-1 -2 -1] [-1 -2 -2])
; ([1 5 1 1] [5 1 1] [1 1 1] [1 5 1] [1 5 1])
; ([-2 -1 -4 -1] [-1 -4 -1] [-2 -4 -1] [-2 -1 -1] [-2 -1 -4])
; ([2 -1 2 1] [-1 2 1] [2 2 1] [2 -1 1] [2 -1 2])
; ([-2 -2 0 -3] [-2 0 -3] [-2 0 -3] [-2 -2 -3] [-2 -2 0])
; ([2 3 1 2] [3 1 2] [2 1 2] [2 3 2] [2 3 1])

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


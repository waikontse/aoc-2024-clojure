(ns advent-of-code-2024.week1.day2
  (:require [advent-of-code-2024.utils.io :as io]
            [clojure.string :as str]
            [clojure.core.reducers :as r]))

(defn parse-line
  [raw-line]
  (->> (str/split raw-line #" ")
       (map io/str->int))
  )

(defn calc-diff
  [arglist]
  (- (first arglist) (second arglist)))

(defn is-strictly?
  [f my-comp max numbers]
  (let [less-then-max (map #(my-comp % max) numbers)
        are-strictly-numbers (map f numbers)
        ; _ (println are-strictly-numbers)
        combined (into [] (r/flatten (map vector less-then-max are-strictly-numbers)))
        ;     _ (println numbers)
        ;    _ (println "combined: " combined)
        result (every? identity combined)
        ]
    ()
    result))

(defn is-safe?
  [numbers]
  (let [diff-nums (->> numbers
                       (partition 2 1)
                       (map calc-diff))
        is-positively-safe (is-strictly? pos-int? <= 3 diff-nums)
        is-negatively-safe (is-strictly? neg-int? >= -3 diff-nums)
        ]
    (or is-positively-safe is-negatively-safe))
  )

(defn solve-part-1
  [filename]
  (println "solving part 1 with filename: " filename)
  (let [raw-lines (io/read-input filename)
        parsed-lines (map parse-line raw-lines)
        all-safety-values (map is-safe? parsed-lines)
        ]
    (count (filter true? all-safety-values))
    ))

(defn solve-part-2
  [filename]
  (let [data (io/read-input filename)
        clean-data (->>
                     data
                     (map #(str/split % #" "))
                     (map #(map io/str->int %))
                     (map #(partition 2 1 %)))
        calc-data (map #(map calc-diff %) clean-data)
        is-strictly-positive (count (filter #(is-strictly? pos-int? <= 3 %) calc-data))
        _ (println calc-data)
        _ (println "positive: " is-strictly-positive)
        is-strictly-negative (count (filter #(is-strictly? neg-int? >= -3 %) calc-data))
        ]
    (+ is-strictly-negative is-strictly-positive)))


;;(def sample (slurp "resources/day2/example.txt"))

;; Solution from Thomas

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


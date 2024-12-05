(ns advent-of-code-2024.week1.day2
  (:require [advent-of-code-2024.utils.io :as io]
            [clojure.string :as str]
            [clojure.core.reducers :as r]))

(defn parse-line
  [raw-line]
  (->> (str/split raw-line #" ")
       (mapv io/str->int)))

(defn calc-diff
  [arglist]
  (- (first arglist) (second arglist)))

(defn is-strictly?
  [f my-comp max numbers]
  (let [less-then-max (map #(my-comp % max) numbers)
        are-strictly-numbers (map f numbers)
        combined (into [] (r/flatten (map vector less-then-max are-strictly-numbers)))
        result (every? identity combined)
        ]
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

(defn calc-diff
  [arglist]
  (- (second arglist) (first arglist)))


(defn solve-part-1
  [filename]
  (println "solving part 1 with filename: " filename)
  (let [raw-lines (io/read-input "day2/example.txt")
        parsed-lines (map parse-line raw-lines)
        all-safety-values (map is-safe? parsed-lines)
        ]
    (count (filter true? all-safety-values)))
  )


(defn generate-rows
  [numbers]
  (let [length (count numbers)
        ]
    (->
      (mapv #(io/vec-remove % numbers) (range length))
      (conj numbers))))
(defn is-safe-part2?
  [numbers]
  (let [gens (generate-rows numbers)
        all-safety-values (mapv is-safe? gens)
        ]
    (some true? all-safety-values)))

(defn solve-part-2
  [filename]
  (println "solving part 2 with filename: " filename)
  (let [raw-lines (io/read-input "day2/input.txt")
        parsed-lines (mapv parse-line raw-lines)
        all-safety-values (mapv is-safe-part2? parsed-lines)
        ]
    (count (filter true? all-safety-values))))
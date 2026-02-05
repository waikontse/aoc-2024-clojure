(ns advent-of-code-2025.week2.day9
  (:require [advent-of-code-2024.utils.io :as io])
  (:require [clojure.math.combinatorics :as combi])
  (:require [advent-of-code-2024.utils.algorithms :as algo]
            [clojure.math.combinatorics :as combo])
  )

(def example (slurp "./resources/y2025/day9/example.txt"))
(def input (slurp "./resources/y2025/day9/input.txt"))

(defn parse-raw-line-to-point
  [raw-line]
  (let [coor (io/strs->ints (clojure.string/split raw-line #","))
        x (first coor)
        y (second coor)
        ]
    [x y]))

(defn parse-raw-lines-to-points
  [raw-lines]
  (vec (map #(parse-raw-line-to-point %) raw-lines)))


(defn calc-area
  [[x1 y1] [x2 y2]]
  (let [abs-x (inc (abs (- x2 x1)))
        abs-y (inc (abs (- y2 y1)))
        ]
    (* abs-x abs-y))
  )

(defn calc-areas
  [areas-xs]
  (map #(calc-area (first %) (second %)) areas-xs)
  )

(defn solve-part-1
  []
  (let [split-lines (clojure.string/split-lines input)
        points (parse-raw-lines-to-points split-lines)
        result (->> (combo/combinations points 2)
                    (calc-areas)
                    )
        ]
    (apply max result))
  )

(solve-part-1)


;;; Code for part 2

(defn generate-vertical
  "docstring"
  [arglist]
  )

(defn generate-horizontal
  "docstring"
  [arglist]
  )

(defn generate-vertical-dots
  [from to]
  (let [[x1 y1] from
        [_ y2] to
        y-min (min y1 y2)
        y-max (max y1 y2)]
    (for [y (range y-min (inc y-max))]
      [x1 y])
    ))

(defn generate-horizontal-dots
  [from to]
  (let [[x1 y1] from
        [x2 _] to
        x-min (min x1 x2)
        x-max (max x1 x2)]
    (for [x (range x-min (inc x-max))]
      [x y1])
    ))

(defn generate-all-dots-for-points
  [from to]
  (let [[x1 y1] from
        [x2 y2] to
        ]
    (cond
      (= x1 x2) (generate-vertical-dots from to)
      (= y1 y2) (generate-horizontal-dots from to)
      :else '()
      )
    )
  )

(generate-all-dots-for-points [1 10] [11 11])

(defn all-points-in-polygon?
  [points points-of-polygon]
  (loop [remaining-points points
         is-in-polygon? true]
    (cond
      (false? is-in-polygon?) false
      (empty? remaining-points) true
      :else
      (recur (rest remaining-points) (algo/in-polygon? (first remaining-points) points-of-polygon))
      )
    )
  )

(defn area-if-all-points-in-polygon
  [points points-of-polygon]
  (if (true? (all-points-in-polygon? points points-of-polygon)))
  )

(defn solve-part-2
  [input-lines]
  (let [split-lines (clojure.string/split-lines input-lines)
        points (parse-raw-lines-to-points split-lines)
        looped-points (conj points (first points))
        all-combinations (combo/combinations points 2)
        _ (println (count all-combinations) (first all-combinations))
        ]
    0)
  )
(solve-part-2 example)


(def input-points-looped
  (->> (clojure.string/split-lines input)
       (parse-raw-lines-to-points)
       (#(conj % (first %)))
       )
  )

(all-points-in-polygon? [[13150 80164]] input-points-looped)


(def simple-square [[1 1] [5 1] [5 5] [1 5] [1 1]])
;
(algo/in-polygon? [5 6] simple-square)
;(algo/is-on-polygon-edge? [0 0] simple-square)
;
;;(algo/is-ccw? [2 4] [1 1] [3 3])
;(algo/angle [2 4] [1 1] [3 3])


(combo/combinations [1 2 3] 2)
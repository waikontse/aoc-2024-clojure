(ns advent-of-code-2025.week2.day9
  (:require [advent-of-code-2024.utils.algorithms :as algo]
            [advent-of-code-2024.utils.io :as io]
            [clojure.math.combinatorics :as combo])
  )
(def example (slurp "./resources/y2025/day9/example.txt"))
(def example2 (slurp "./resources/y2025/day9/example2.txt"))
(def example3 (slurp "./resources/y2025/day9/example3.txt"))
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
    (* abs-x abs-y)))

(defn calc-areas
  [areas-xs]
  (map #(calc-area (first %) (second %)) areas-xs))

(defn solve-part-1
  []
  (let [split-lines (clojure.string/split-lines input)
        points (parse-raw-lines-to-points split-lines)
        result (->> (combo/combinations points 2)
                    (calc-areas)
                    )
        ]
    (apply max result)))

(solve-part-1)

;;; Code for part 2

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

(defn generate-square-dots
  [from to]
  (let [[x1 y1] from
        [x2 y2] to
        x-min (min x1 x2)
        x-max (max x1 x2)
        y-min (min y1 y2)
        y-max (max y1 y2)
        top-left [(inc x-min) (inc y-min)]  ;(generate-vertical-dots [x-min y-min] [x-min y-max])
        top-right [(dec x-max) (inc y-min)] ;(generate-vertical-dots [x-max y-min] [x-max y-max])
        bottom-left [(inc x-min) (dec y-max)] ;(generate-horizontal-dots [(inc x-min) y-min] [(dec x-max) y-min])
        bottom-right [(dec x-max) (dec y-max)] ;(generate-horizontal-dots [(inc x-min) y-max] [(dec x-max) y-max])
        ]
     [top-left top-right bottom-right bottom-left top-left]
    )
  )

(generate-square-dots [1 1] [4 4])

(defn generate-all-dots-for-points
  [from to]
  (let [[x1 y1] from
        [x2 y2] to
        ]
    (cond
      (= x1 x2) [] ; (generate-vertical-dots from to)
      (= y1 y2) [] ; (generate-horizontal-dots from to)
      :else (generate-square-dots from to)
      )
    )
  )

(generate-all-dots-for-points [1 10] [11 11])

;; precheck-all 4 corners first, before checking the rest


;; should make faster with pmap implementation
(defn all-points-in-polygon?
  [points points-of-polygon with-edge]
  (cond
    (empty? points) false
    :else
    (loop [remaining-points points
           is-in-polygon? true]
      (cond
        (false? is-in-polygon?) false
        (empty? remaining-points) true
        :else
        (recur (rest remaining-points) (algo/in-polygon? (first remaining-points) points-of-polygon with-edge))
        )
      ))
  )


;; 1. Check if all corners are inside
;; 2. Check if the "smaller" points are inside
;; 3. Check if there are points inside the square
;; 4. Are there lines crossing the rectangle?
(defn all-corners-are-inside?
  [corners points-of-polygon]
  (all-points-in-polygon? corners points-of-polygon true)
  )

(defn all-points-are-outside?
  "Check that all the points of the polygon are outside the corner. Edge detection should be turned off."
  [points-of-polygon corners]
  (cond
    (empty? corners) true
    :else
    (loop [remaining-points points-of-polygon
           is-in-polygon? false]
      (cond
        (true? is-in-polygon?) false
        (empty? remaining-points) true
        :else
        (recur (rest remaining-points) (algo/in-polygon? (first remaining-points) corners false))
        )
      ))
  )

(defn combination-area
  "Check if the combination of from to is inside the polygon and calculate the area if inside."
  [combination points-of-polygon]
  (let [first-coor (first combination)
        second-coor (second combination)
        corners (generate-all-dots-for-points first-coor second-coor)
        ;_ (println "middle point:" middle-point)
        ;_ (println "new corners:" new-corners)
        corners-inside? (all-corners-are-inside? corners points-of-polygon)
        ]
    (cond
      (false? corners-inside?) 0
      :else
      (let [all-polygon-points-are-outside? (all-points-are-outside? points-of-polygon corners)
            area (calc-area first-coor second-coor)
            _ (println "is inside:" all-polygon-points-are-outside? first-coor second-coor "area:" area)
            ]
        (if (true? all-polygon-points-are-outside?)
          area
          0)
        )
      )
    )
  )

(defn max-area-if-all-points-in-polygon
  [combinations points-of-polygon]
  (->> (map #(combination-area % points-of-polygon) combinations)
       (reduce max))
  )

;; Answer part 1: 4715966250
                  4601733120
;; Failed: 4601733120 - Too high
           4601733120

(defn solve-part-2
  [input-lines]
  (let [split-lines (clojure.string/split-lines input-lines)
        points (parse-raw-lines-to-points split-lines)
        looped-points (conj points (first points))
        partitioned-looped-points (partition 2 1 looped-points)

        ;all-combinations (combo/combinations points 2)
        _ (println "combinations counts:" (count partitioned-looped-points))
        horizontal-scaling (->> (filter #(is-horizontal? (first %) (second %)) partitioned-looped-points)
                                (map #(horizontal-dist (first %) (second %)))
                                (filter #(not (is-prime? %)))
                                (filter #(even? %))
                                ;(reduce math/gcd)
                                )
        ;;_ (println "big numbers: " (reduce * (into #{} horizontal-scaling)))
        _ (println "count:" (count horizontal-scaling) "horizontal scaling:" horizontal-scaling)
        ;_ (println (count all-combinations)  all-combinations)
        ;max-area (max-area-if-all-points-in-polygon all-combinations looped-points)
        max-gcd (try-to-find-highest-gcd horizontal-scaling)
        _ (println "max gcd:" max-gcd)
        ;_ (println "max area: " max-area)
        ]
    0)
  )
(time (solve-part-2 input))

(def input-points-looped
  (->> (clojure.string/split-lines input)
       (parse-raw-lines-to-points)
       (#(conj % (first %)))
       )
  )

(def example-points-looped
  (->> (clojure.string/split-lines example)
       (parse-raw-lines-to-points)
       (#(conj % (first %)))
       )
  )

(def example2-points-looped
  (->> (clojure.string/split-lines example2)
       (parse-raw-lines-to-points)
       (#(conj % (first %)))
       )
  )

(def example3-points-looped
  (->> (clojure.string/split-lines example3)
       (parse-raw-lines-to-points)
       (#(conj % (first %)))
       )
  )

(solve-part-2 example3)

;(all-points-in-polygon? [[13150 80164]] example-points-looped true)

(def simple-square [[1 1] [5 1] [5 5] [1 5] [1 1]])
;
(algo/in-polygon? [5 5] simple-square false)
;(algo/is-on-polygon-edge? [0 0] simple-square)
;
;;(algo/is-ccw? [2 4] [1 1] [3 3])
;(algo/angle [2 4] [1 1] [3 3])


(combo/combinations [1 2 3] 2)
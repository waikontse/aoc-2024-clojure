(ns advent-of-code-2025.week2.day9
  (:require [advent-of-code-2024.utils.algorithms :as algo]
            [advent-of-code-2024.utils.io :as io]
            [clojure.math.combinatorics :as combo])
  )
(def example (slurp "./resources/y2025/day9/example.txt"))
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
(defn generate-square-dots
  [from to]
  (let [[x1 y1] from
        [x2 y2] to
        x-min (min x1 x2)
        x-max (max x1 x2)
        y-min (min y1 y2)
        y-max (max y1 y2)
        top-left [x-min y-min]
        top-right [x-max y-min]
        bottom-left [x-min y-max]
        bottom-right [x-max y-max]
        ]
    [top-left top-right bottom-right bottom-left top-left]
    ))

(defn generate-all-dots-for-points
  [from to]
  (let [[x1 y1] from
        [x2 y2] to
        ]
    (cond
      (= x1 x2) []
      (= y1 y2) []
      :else (generate-square-dots from to)
      )))

(def memo-in-polygon? (memoize algo/in-polygon?))

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
        (recur (rest remaining-points) (memo-in-polygon? (first remaining-points) points-of-polygon with-edge))
        ))))


;; 1. Check if all corners are inside
;; 2. Check if there are points inside the square
;; 3. Are there lines crossing the rectangle vertically?
;; 4. Are there lines crossing the rectangle horizontally?
(defn all-corners-are-inside?
  [corners points-of-polygon]
  (all-points-in-polygon? corners points-of-polygon true)
  )

(defn crosses-rectangle-vertically-single?
  [line-from line-to rectangle-from rectangle-to]
  (let [[xline1 yline1] line-from
        [xline2 yline2] line-to
        [xrect1 yrect1] rectangle-from
        [xrect2 yrect2] rectangle-to
        x-min (min xrect1 xrect2)
        x-max (max xrect1 xrect2)
        y-min (min yrect1 yrect2)
        y-max (max yrect1 yrect2)
        ]
    (cond
      (not= xline1 xline2) false
      (or (<= xline1 x-min) (>= xline1 x-max)) false
      (and (<= yline1 y-min) (<= yline2 y-min)) false
      (and (>= yline1 y-max) (>= yline2 y-max)) false
      :else true)))

(defn polygon-pairs [points]
  (vec (partition 2 1 points)))

(def memo-polygon-pairs (memoize polygon-pairs))


(defn crosses-rectangle-vertically?
  [from to points-of-polygon]
  (loop [combinations (memo-polygon-pairs points-of-polygon)
         crosses-vertically? false
         ]
    (cond
      (empty? combinations) false
      (true? crosses-vertically?) true
      :else
      (let [combo (first combinations)]
        (recur (rest combinations) (crosses-rectangle-vertically-single? (first combo) (second combo) from to))
        ))))

(defn crosses-rectangle-horizontally-single?
  [line-from line-to rectangle-from rectangle-to]
  (let [[xline1 yline1] line-from
        [xline2 yline2] line-to
        [xrect1 yrect1] rectangle-from
        [xrect2 yrect2] rectangle-to
        x-min (min xrect1 xrect2)
        x-max (max xrect1 xrect2)
        y-min (min yrect1 yrect2)
        y-max (max yrect1 yrect2)
        ]
    (cond
      (not= yline1 yline2) false
      (or (<= yline1 y-min) (>= yline1 y-max)) false
      (and (<= xline1 x-min) (<= xline2 x-min)) false
      (and (>= xline1 x-max) (>= xline2 x-max)) false
      :else true)))

(defn crosses-rectangle-horizontally?
  [from to point-of-polygon]
  (loop [combinations (memo-polygon-pairs point-of-polygon)
         crosses-horizontally? false
         ]
    (cond
      (empty? combinations) false
      (true? crosses-horizontally?) true
      :else
      (let [combo (first combinations)]
        (recur (rest combinations) (crosses-rectangle-horizontally-single? (first combo) (second combo) from to))
        ))))

(defn all-polygon-points-are-outside-rectangle?
  "Check that all the points of the polygon are outside the corner. Edge detection should be turned off.
   Returns true "
  [corners points-of-polygon]
  (cond
    (empty? corners) true
    :else
    (loop [remaining-polygon-points points-of-polygon
           is-in-rectangle? false]
      (cond
        (true? is-in-rectangle?) false
        (empty? remaining-polygon-points) true
        :else
        (recur (rest remaining-polygon-points) (memo-in-polygon? (first remaining-polygon-points) corners false))
        )
      ))
  )

(defn combination-area
  "Check if the combination of from to is inside the polygon and calculate the area if inside."
  [combination points-of-polygon]
  (let [first-coor (first combination)
        second-coor (second combination)
        corners (generate-all-dots-for-points first-coor second-coor)
        corners-inside? (all-corners-are-inside? corners points-of-polygon)
        ]
    (cond
      (false? corners-inside?) 0
      (true? (crosses-rectangle-horizontally? first-coor second-coor points-of-polygon)) 0
      (true? (crosses-rectangle-vertically? first-coor second-coor points-of-polygon)) 0
      :else
      (let [all-polygon-points-are-outside? (all-polygon-points-are-outside-rectangle? corners points-of-polygon)
            area (calc-area first-coor second-coor)
            ]
        (if (true? all-polygon-points-are-outside?)
          area
          0)))))

(defn max-area-if-all-points-in-polygon
  [combinations points-of-polygon]
  (->> combinations
       (pmap #(future (combination-area % points-of-polygon)))
       (pmap deref)
       (reduce max)))

;; 1530527040
(defn solve-part-2
  [input-lines]
  (let [split-lines (clojure.string/split-lines input-lines)
        points (parse-raw-lines-to-points split-lines)
        looped-points (conj points (first points))
        all-combinations (combo/combinations points 2)
        max-area (max-area-if-all-points-in-polygon all-combinations looped-points)
        _ (println "max area: " max-area)
        ]
    max-area)
  )
(time (solve-part-2 input))




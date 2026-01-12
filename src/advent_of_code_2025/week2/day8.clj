(ns advent-of-code-2025.week2.day8
  (:require [advent-of-code-2024.utils.io :as io]))


(def example (slurp "./resources/y2025/day8/example.txt"))
(def input (slurp "./resources/y2025/day8/input.txt"))

(defn calc-distance
  [point-from point-to]
  (let [diff-x (- (:x point-from) (:x point-to))
        diff-y (- (:y point-from) (:y point-to))
        diff-z (- (:z point-from) (:z point-to))
        dist-xy (Math/sqrt (+ (* diff-x diff-x) (* diff-y diff-y)))
        total-dist (Math/sqrt (+ (* diff-z diff-z) (* dist-xy dist-xy)))
        ;;_ (println "Distance from:" point-from "to" point-to " ->" total-dist)
        ]
    total-dist))

(defn calc-distance-all-pairs
  [coll]
  (map-indexed (fn [idx item]
                 (let [rest (drop (inc idx) coll)
                       all-distances (map #(calc-distance item %) rest)
                       ]
                   {:source item :distances all-distances})
                 )
               coll)
  )

(defn connect-junction-boxes
  [coll max-connections]
  (loop [prioritized-dist 0
         seen #{}
         connected-boxes {}
         connected-count 0
         ]
    (let [curr (first prioritized-dist)
          ]
      (cond
        (= max-connections connected-count) connected-boxes
        (empty? prioritized-dist) (throw (Exception. "Not expected. Limit not reached, but targets are finished."))
        (or (contains? seen (:from curr)) (contains? seen (:to curr))) (recur (rest prioritized-dist) seen connected-boxes connected-count)
        :else (throw (Exception. "Implement me"))        )
      )
    )
  )

(calc-distance
  {:x 0 :y 0 :z 0}
  {:x 1 :y 1 :z 1})

(defn parse-line-to-point
  [raw-line]
  (let [splitted (clojure.string/split raw-line #",")
        [x y z] (io/strs->ints splitted)
        ]
    {:x x :y y :z z}))

(defn parse-lines-to-points
  [raw-lines]
  (map #(parse-line-to-point %) raw-lines))



(defn solve-part-1
  [input]
  (let [raw-lines (clojure.string/split-lines input)
        points (parse-lines-to-points raw-lines)
        _ (clojure.pprint/pprint points)
        all-distances (calc-distance-all-pairs points)
        _ (clojure.pprint/pprint all-distances)
        ]
    )
  )

(solve-part-1 example)
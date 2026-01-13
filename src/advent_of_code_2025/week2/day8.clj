(ns advent-of-code-2025.week2.day8
  (:require [advent-of-code-2024.utils.io :as io]))


(def example (slurp "./resources/y2025/day8/example.txt"))
(def input (slurp "./resources/y2025/day8/input.txt"))

(defn calc-distance
  [point-from point-to]
  (let [diff-x (- (:x point-from) (:x point-to))
        diff-y (- (:y point-from) (:y point-to))
        diff-z (- (:z point-from) (:z point-to))
        total-dist (Math/sqrt (+ (* diff-z diff-z) (* diff-x diff-x) (* diff-y diff-y)))
        ]
    {:from point-from :to point-to :distance total-dist}))

(defn calc-distance-all-pairs
  [coll]
  (map-indexed (fn [idx item]
                 (let [rest (drop (inc idx) coll)
                       all-distances (map #(calc-distance item %) rest)
                       ]
                   all-distances))
               coll)
  )

(defn connect-junction-boxes
  [coll max-connections]
  (loop [prioritized-dist coll
         seen {}
         connected-boxes {}
         connected-count 0]
    (cond
      (= max-connections connected-count) connected-boxes
      (empty? prioritized-dist) (throw (Exception. "Not expected. Limit not reached, but targets are finished."))

      :else
      (let [curr (first prioritized-dist)
            from (:from curr)
            to (:to curr)
            from-is-seen? (contains? seen from)
            to-is-seen? (contains? seen to)
            from-connected-to (get seen from)
            to-connected-to (get seen to)
            _ (clojure.pprint/pprint connected-boxes)
            _ (clojure.pprint/pprint seen)
            _ (println "connected-count:" connected-count)
            ]
        (cond
          ;; :from is connected, and :to is connected -> continue without doing anything
          (and from-is-seen? to-is-seen?) (recur
                                            (rest prioritized-dist)
                                            seen
                                            connected-boxes
                                            connected-count)
          ;; Connect when :from is connected, and :to is unconnected -> connect to from
          (and from-is-seen? (false? to-is-seen?)) (recur
                                                     (rest prioritized-dist)
                                                     (assoc seen to from-connected-to)
                                                     (assoc connected-boxes from-connected-to (conj (get connected-boxes from-connected-to) to))
                                                     (inc connected-count))
          ;; Connect when :from is unconnected, and to is connected
          (and (false? from-is-seen?) to-is-seen?) (recur
                                                     (rest prioritized-dist)
                                                     (assoc seen from to-connected-to)
                                                     (assoc connected-boxes to-connected-to (conj (get connected-boxes to-connected-to) from))
                                                     (inc connected-boxes))

          ;; Connect when :from is unconnected, and :to is unconnected
          (and (false? from-is-seen?) (false? to-is-seen?)) (recur
                                                              (rest prioritized-dist)
                                                              (assoc seen from from to from)
                                                              (assoc connected-boxes (:from curr) (conj (get connected-boxes (:from curr)) (:to curr)))
                                                              (+ connected-count 2))
          )
        )
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
        all-distances-combined (flatten all-distances)
        ;_ (clojure.pprint/pprint all-distances-combined)
        sorted-all-distances-combined (sort-by #(:distance %) all-distances-combined)
        _ (clojure.pprint/pprint sorted-all-distances-combined)
        connected (connect-junction-boxes sorted-all-distances-combined 10)
        _ (clojure.pprint/pprint connected)
        ]
    0)
  )

(solve-part-1 example)
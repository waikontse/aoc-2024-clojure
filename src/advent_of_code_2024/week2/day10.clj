(ns advent-of-code-2024.week2.day10
  (:require [advent-of-code-2024.utils.io :as io]
            [advent-of-code-2024.utils.board :as b]
            [clojure.pprint :as pp]))

;; START - GLOBAL definitions
(def trailhead 0)
(def mountain-top 9)


;; END - GLOBAL definitions

(defn find-all-trailheads
  "Find all the positions of the trailheads.
   Returning a collection of positions {:x-position :y-position}"
  [board]
  (let [all-matched-symbols (for [x-pos (range (:width board))
                                  y-pos (range (:height board))
                                  ]
                              (when (zero? (b/get-pos x-pos y-pos board))
                                {:x-pos x-pos :y-pos y-pos}))
        ]
    (filter some? all-matched-symbols)))

(defn is-valid-next-step?
  [a b]
  (let [b-val (if (some? b)
                (last b)
                -1)
        ]
    (= (inc a) b-val)))

(defn find-next-steps
  "Find the next steps on the map for the given position
  board: the map we need to traverse
  pos: the position on the map
  f: the fitness function for a valid next step"
  [board pos f]
  (let [current-number (b/get-pos (:x-pos pos) (:y-pos pos) board)
        top-number (when (b/can-get-data-top? (:y-pos pos) board 2)
                     (b/get-data-top (:x-pos pos) (:y-pos pos) board 2))
        bottom-number (when (b/can-get-data-bottom? (:y-pos pos) board 2)
                        (b/get-data-bottom (:x-pos pos) (:y-pos pos) board 2))
        left-number (when (b/can-get-data-left? (:x-pos pos) board 2)
                      (b/get-data-left (:x-pos pos) (:y-pos pos) board 2))
        right-number (when (b/can-get-data-right? (:x-pos pos) board 2)
                       (b/get-data-right (:x-pos pos) (:y-pos pos) board 2))

        next-valid-positions [(when (f current-number top-number) (b/get-pos-top pos))
                              (when (f current-number bottom-number) (b/get-pos-bottom pos))
                              (when (f current-number left-number) (b/get-pos-left pos))
                              (when (f current-number right-number) (b/get-pos-right pos))
                              ]
        ]
    (filter some? next-valid-positions)))

(defn is-mountain-top?
  [board pos]
  (let [pos-value (b/get-pos (:x-pos pos) (:y-pos pos) board)]
    (= mountain-top pos-value)))

(defn find-all-reachable-tops-from-trailhead
  "Find all the reachable mountain tops given a trailhead and a map.
   Only returning the number of unique mountain tops."
  [m start-pos start-coll]
  (loop [next-positions [start-pos]
         mountain-tops start-coll
         ]
    (if (empty? next-positions)
      mountain-tops
      (let [current-position (first next-positions)
            updated-mountain-tops (if (is-mountain-top? m current-position)
                                    (conj mountain-tops current-position)
                                    mountain-tops)
            updated-positions (->> (find-next-steps m current-position is-valid-next-step?)
                                   (into (rest next-positions)))
            ]
        (recur updated-positions updated-mountain-tops))
      )
    ))


(defn solve
  [filename start-coll]
  (let [raw-lines (io/read-input filename)
        raw->int-lines (->> raw-lines
                            (map io/chars->strs)
                            (map io/strs->ints))
        parsed-board (b/parse-to-board raw->int-lines)
        _ (b/print-board parsed-board)
        trailheads (find-all-trailheads parsed-board)
        trailheads->tops (map #(find-all-reachable-tops-from-trailhead parsed-board % start-coll) trailheads)
        ]
    (reduce + (map count trailheads->tops)))
  )

(defn solve-part-1
  []
  (solve "day10/example.txt" #{}))

(defn solve-part-2
  []
  (solve "day10/input.txt" []))

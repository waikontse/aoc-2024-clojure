(ns advent-of-code-2025.week2.day9
  (:require [advent-of-code-2024.utils.io :as io])
  (:require [advent-of-code-2024.utils.board :as board])
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

(defn make-combinations
  [points]
  (map-indexed (fn [idx item]
                 (let [rest (drop (inc idx) points)]
                   (map #(calc-area item %) rest)))
               points)
  )

(defn solve-part-1
  []
  (let [split-lines (clojure.string/split-lines input)
        points (parse-raw-lines-to-points split-lines)
        ;_ (println points)
        result (flatten (make-combinations points))
        ;_ (println result)
        ]
    (apply max result))
  )
(solve-part-1)


;;; Code for part 2

(def EMPTY \.)
(def RED \#)
(def GREEN \X)

(defn create-matching-board
  "docstring"
  [points]
  (let [max-x (apply max (map #(first %) points))
        max-y (apply max (map #(second %) points))
        _ (println max-x max-y)
        row-data (repeat (inc max-x) EMPTY)
        full-board-data (repeat (inc max-y) row-data)
        ]
    (board/parse-to-board full-board-data))
  )

(defn create-connection-points
  [points]
  (->> (first points)
       (conj points)
       (partition 2 1)))

(defn connect-horizontal-green-dots
  [x y x-max board]
  (if (< x-max x)
    board
    (connect-horizontal-green-dots (inc x) y x-max (board/set-pos x y board GREEN)))
  )

(defn connect-vertical-green-dots
  [x y y-max board]
  (if (< y-max y)
    board
    (connect-vertical-green-dots x (inc y) y-max (board/set-pos x y board GREEN)))
  )

(defn connect-green-dots
  [x1 y1 x2 y2 board]
  (let [x-min (min x1 x2)
        x-max (max x1 x2)
        y-min (min y1 y2)
        y-max (max y1 y2)]
    (cond
      (= x1 x2) (connect-vertical-green-dots x1 (inc y-min) (dec y-max) board)
      (= y1 y2) (connect-horizontal-green-dots (inc x-min) y1 (dec x-max) board)
      :else (throw (Exception. "Unexpected points direction"))
      )
    )
  )

(defn connect-points-on-board
  [points board]
  (loop [remaining-points points
         updated-board board]
    (cond
      (empty? remaining-points) updated-board
      :else
      (let [[[x1 y1] [x2 y2]] (first remaining-points)
            updated-board-with-reds-1 (board/set-pos x1 y1 updated-board RED)
            updated-board-with-reds-2 (board/set-pos x2 y2 updated-board-with-reds-1 RED)
            updated-board-with-greens (connect-green-dots x1 y1 x2 y2 updated-board-with-reds-2)
            ]
        (recur (rest remaining-points) updated-board-with-greens))
      )
    )
  )

(defn solve-part-2
  [input]
  (let [split-lines (clojure.string/split-lines input)
        points (parse-raw-lines-to-points split-lines)
        connection-points (create-connection-points points)
        ;_ (println points)
        _ (clojure.pprint/pprint connection-points)
        ;board (create-matching-board points)
        ;new-board (connect-points-on-board connection-points board)
        ;_ (board/print-board new-board)
        ]
    0)
  )
(solve-part-2 input)

(for [x (range 1 2)
      y (range 1 3)]
  (println x y))

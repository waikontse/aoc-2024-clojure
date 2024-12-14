(ns advent-of-code-2024.week1.day6
  (:require [advent-of-code-2024.utils.io :as io])
  (:require [advent-of-code-2024.utils.board :as board]))

(def obstacle \#)
(def marked \X)

(defn mark-position
  "docstring"
  [board current-position]
  (board/set-pos (:xPos current-position) (:yPos current-position) board marked))

(defn determine-next-position
  "docstring"
  [current-position direction]
  (let [new-x-pos (cond
                    (= :WEST direction) (dec (:xPos current-position))
                    )
        new-y-pos 0]
    {:xPos new-x-pos :yPos new-y-pos}))

(defn can-move?
  "docstring"
  [board current-position direction]
  (let [new-x-pos 0
        new-y-pos 0
        is-off-board (board/is-off-board? new-x-pos new-y-pos board)]
    (cond
      (true? is-off-board) false
      (= direction :NORTH)
      (= direction :SOUTH)
      (= direction :WEST)
      (= direction :EAST)
      ))
  )

(defn solve-part-1
  "docstring"
  [filenme]
  (let [raw-lines (io/read-input "day6/example.txt")
        board (board/parse-to-board raw-lines)
        _ (println board)
        ]
    )
  0)

(defn solve-part-2
  "docstring"
  [filename]
  )
(ns advent-of-code-2024.week3.day15
  (:require [advent-of-code-2024.utils.io :as io]
            [advent-of-code-2024.utils.board :as b]
            [clojure.pprint :as pp]))

(defn parse-raw-lines-to-board
  [raw-lines]
  (let [raw-board-lines (filter #(clojure.string/starts-with? % "#") raw-lines)
        board (b/parse-to-board raw-board-lines)
        ]
    board))

(defn parse-raw-lines-to-instructions
  [raw-lines]
  (let [raw-instructions (drop-while #(or (clojure.string/starts-with? % "#")
                                          (clojure.string/blank? %))
                                     raw-lines)]
    (flatten raw-instructions)))

(def starter \@)

(defn find-starting-position
  [board]
  (for [x-pos (range (:width board))
        y-pos (range (:height board))
        :when (= starter (b/get-pos x-pos y-pos board))
        ]
    {:x-pos x-pos :y-pos y-pos})
  )

(defn can-move?
  [board curr-pos direction]
  "TODO")

(defn move
  [board curr-pos direction]
  "TODO")

(defn solve-part-1
  ""
  [filename]
  (let [raw-lines (io/read-input "day15/example.txt")
        board (parse-raw-lines-to-board raw-lines)
        start-pos (first (find-starting-position board))
        _ (b/print-board board)
        _ (println start-pos)
        instructions (parse-raw-lines-to-instructions raw-lines)
        _ (println "move instructions: " instructions)
        ]
    0)
  )
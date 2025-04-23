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
  )



(defn solve-part-1
  ""
  [filename]
  (let [raw-lines (io/read-input "day15/example.txt")
        board (parse-raw-lines-to-board raw-lines)
        _ (b/print-board board)]
    0)
  )

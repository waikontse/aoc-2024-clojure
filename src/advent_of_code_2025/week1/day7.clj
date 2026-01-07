(ns advent-of-code-2025.week1.day7
  (:require [advent-of-code-2024.utils.board :as board]))

(def example (slurp "./resources/y2025/day7/example.txt"))
(def input (slurp "./resources/y2025/day7/input.txt"))
(def start \S)
(def splitters \^)


(defn solve-part-1
  [input]
  (let [raw-lines (clojure.string/split-lines input)
        parsed-board (board/parse-to-board raw-lines)
        _ (clojure.pprint/pprint parsed-board)
        ]
    )
  )

example

(solve-part-1 example)

(defn solve-part-2
  [input]
  )

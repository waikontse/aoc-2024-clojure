(ns advent-of-code-2025.week1.day7
  (:require [advent-of-code-2024.utils.board :as board]))

(def example (slurp "./resources/y2025/day7/example.txt"))
(def input (slurp "./resources/y2025/day7/input.txt"))
(def start \S)
(def splitter \^)

(defn solve-part-1
  [input]
  (let [raw-lines (clojure.string/split-lines input)
        parsed-board (board/parse-to-board raw-lines)
        starting-pos (board/find-all-chars-in-board parsed-board start)
        tachyons (board/find-all-chars-in-board parsed-board splitter)
        ;;_ (clojure.pprint/pprint parsed-board)
        _ (println "starting pos:" starting-pos)
        _ (println "tachyons:" tachyons)]))

(solve-part-1 example)

(defn solve-part-2
  [input])

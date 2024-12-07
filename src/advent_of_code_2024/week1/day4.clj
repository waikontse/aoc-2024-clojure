(ns advent-of-code-2024.week1.day4
  (:require [advent-of-code-2024.utils.io :as io]
            [advent-of-code-2024.utils.board :as board]))

(defn is-target?
  [target suspect]
  (= target suspect))

(defn parse-to-board
  [raw-lines]
  (let [flattened-data (->>
                            (map #(vec (char-array %1)) raw-lines)
                            ;(flatten)
                            (flatten)
                            (vec))
        width (count (first raw-lines))
        height (count raw-lines)
        _ (println "width: " width "height: " height)
        empty-board (board/new width height)
        _ (println empty-board)
        _ (println flattened-data)
        ]
    (board/update-board-data empty-board flattened-data)
    ))

(defn solve-part-1
  [filename]
  (let [raw-lines (io/read-input "day4/example.txt")
        ;_ (println raw-lines)
        filled-board (parse-to-board raw-lines)
        ;_ (println filled-board)
        ]
    )
  0)

(defn solve-part-2
  [filename]
  0)

(seq (char-array "hello"))

(def names ["batman" "superman" "hulk"])

(->>
  (map #(seq (char-array %1)) names)
  (flatten))
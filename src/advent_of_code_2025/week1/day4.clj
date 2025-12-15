(ns advent-of-code-2025.week1.day4
  (:require [advent-of-code-2024.utils.board :as board]))

(def example (slurp "./resources/y2025/day4/example.txt"))
(def input (slurp "./resources/y2025/day4/input.txt"))
(def target \@)

(defn find-all-starter-locations
  [board]
  (for [x (range 0 (:width board))
        y (range 0 (:height board))
        :when (= target (board/get-pos x y board))]
    [x y]))

(defn gather-all-surrounded-by
  [board pos]
  (let [xPos (get pos 0)
        yPos (get pos 1)
        length 2
        N (when (board/can-get-data-top? yPos board length) (board/get-data-top xPos yPos board length))
        NE (when (board/can-get-data-top-right xPos yPos board length) (board/get-data-top-right xPos yPos board length))
        NW (when (board/can-get-data-top-left xPos yPos board length) (board/get-data-top-left xPos yPos board length))
        E (when (board/can-get-data-right? xPos board length) (board/get-data-right xPos yPos board length))
        SE (when (board/can-get-data-bottom-right? xPos yPos board length) (board/get-data-bottom-right xPos yPos board length))
        S (when (board/can-get-data-bottom? yPos board length) (board/get-data-bottom xPos yPos board length))
        SW (when (board/can-get-data-bottom-left? xPos yPos board length) (board/get-data-bottom-left xPos yPos board length))
        W (when (board/can-get-data-left? xPos board length) (board/get-data-left xPos yPos board length))
        ]
    {:rolls (->> [N NE NW E SE S SW W]
                 (map #(get % 1))
                 (flatten))
     :pos pos}
    ))

(defn can-pass-through?
  "expected: {:rolls ... :pos ...}"
  [item]
  (-> (filter #(= \@ %) (:rolls item))
      (count)
      (< 4)))

(defn solve-board
  [board]
  (let [
        all-toilet-rolls (find-all-starter-locations board)
        gathered-around-all-toilet-rolls (map #(gather-all-surrounded-by board %) all-toilet-rolls)
        all-passable-toilet-stacks (filter #(can-pass-through? %) gathered-around-all-toilet-rolls)
        reachable-rolls (count all-passable-toilet-stacks)
        ]
    {:reachable reachable-rolls :board board :posxs (map #(:pos %) all-passable-toilet-stacks)})
  )

(defn solve
  [input]
  (let [splitted-lines (clojure.string/split-lines input)
        parsed-board (board/parse-to-board splitted-lines)
        item (solve-board parsed-board)
        ]
    item))

(defn solve-part-1
  []
  (:reachable (solve example)))

;; 1384
(solve-part-1)

(defn mark-board-with-reached-rolls
  "docstring"
  [item]
  (reduce (fn [board pos]
            (board/set-pos (get pos 0) (get pos 1) board \x))
          (:board item)
          (:posxs item))
  )

(defn solve-until-stopped
  [input]
  (loop [item (solve input)
         acc (:reachable item)
         ]
    (if (= 0 (:reachable item))
      acc
      (let [new-board (mark-board-with-reached-rolls item)
            new-board-item (solve-board new-board)
            ]
        (recur new-board-item (+ acc (:reachable new-board-item)))))
    ))

(defn solve-part-2
  []
  (solve-until-stopped example))

(solve-part-2)
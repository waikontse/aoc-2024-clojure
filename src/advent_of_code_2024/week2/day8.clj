(ns advent-of-code-2024.week2.day8
  (:require [advent-of-code-2024.utils.board :as board]
            [advent-of-code-2024.utils.io :as io]))

(defn find-all-distinct-symbols
  "docstring"
  [board]
  (let [all-symbols (->>
                      (for [x-pos (range (:width board))
                            y-pos (range (:height board))]
                        (let [symbol (board/get-pos x-pos y-pos board)]
                          (when (not= \. symbol)
                            {:symbol symbol :x-pos x-pos :y-pos y-pos})))
                      (filter some?))]
    (group-by :symbol all-symbols)))

(defn generate-anti-nodes
  "docstring"
  [left right]
  (println "generating anti-nodes" left right)
  (let [x-diff (- (:x-pos left) (:x-pos right))
        y-diff (- (:y-pos left (:y-pos right)))
        new-left-position {:x-pos (+ (:x-pos left) x-diff) :y-pos (+ (:y-pos left) y-diff)}
        new-right-position {:x-pos (- (:x-pos right) x-diff) :y-pos (- (:y-pos right) y-diff)}
        ]
    [new-left-position new-right-position]))

(defn generate-all-anti-nodes
  "docstring"
  [positions]
  (println "generating all-anti-nodes" positions)
  (loop [elem1 (first positions)
         xs (rest positions)
         acc []]
    (let [combinations (when (not-empty xs) (map (fn [elem] [elem1 elem]) xs))
          _ (println "combinations:" combinations)]
      (if (empty? xs)
        acc
        (recur (first xs)
               (rest xs)
               (into acc (map #(generate-anti-nodes (first %) (second %)) combinations)))))
    )
  )

(defn solve-part-1
  "docstring"
  [filename]
  (let [raw-lines (io/read-input "day8/example.txt")
        board (board/parse-to-board raw-lines)
        all-distinct-symbols (find-all-distinct-symbols board)]
    (generate-all-anti-nodes (get all-distinct-symbols \0)))
  )


(defn solve-part-2
  "docstring"
  [filename]
  0)
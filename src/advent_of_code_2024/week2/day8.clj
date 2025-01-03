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

(defn generate-anti-nodes-till-out-of-bounds
  [start-x start-y x-diff y-diff board op till-out-of-bounds]
  (let [anti-nodes (atom [])
        current-pos (atom {:x-pos (op start-x x-diff) :y-pos (op start-y y-diff)})]
    (while (board/is-on-board? (:x-pos @current-pos) (:y-pos @current-pos) board)
      (do
        (swap! anti-nodes conj current-pos)
        (swap! current-pos assoc
               :x-pos (op (:x-pos current-pos) x-diff)
               :y-pos (op (:y-pos current-pos) y-diff))))
    anti-nodes)
  )

(defn generate-anti-nodes
  "docstring"
  [left right board till-out-of-bounddo]
  (println "generating anti-nodes" left right)
  (let [x-diff (- (:x-pos left) (:x-pos right))
        y-diff (- (:y-pos left) (:y-pos right))
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
        (flatten acc)
        (recur (first xs)
               (rest xs)
               (into acc (map #(generate-anti-nodes (first %) (second %)) combinations)))))
    )
  )

(defn solve-part-1
  "docstring"
  [filename]
  (let [raw-lines (io/read-input "day8/input.txt")
        board (board/parse-to-board raw-lines)
        all-distinct-symbols (find-all-distinct-symbols board)
        all-anti-nodes (->> all-distinct-symbols
                            (keys)
                            (map #(generate-all-anti-nodes (get all-distinct-symbols %)))
                            flatten
                            set
                            )
        _ (println "all anti nodes" all-anti-nodes)
        filtered (filter #(board/is-on-board? (:x-pos %) (:y-pos %) board) all-anti-nodes)
        on-board-count (println "on boards" (count filtered))
        ]
    ; (generate-all-anti-nodes (get all-distinct-symbols \A))
    on-board-count)
  )


(defn solve-part-2
  "docstring"
  [filename]
  0)
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
  [start-x start-y x-diff y-diff board op]
  (let [anti-nodes (atom [])
        current-pos (atom {:x-pos (op start-x x-diff) :y-pos (op start-y y-diff)})]
    (while (board/is-on-board? (:x-pos @current-pos) (:y-pos @current-pos) board)
      (do
        (swap! anti-nodes conj @current-pos)
        (swap! current-pos assoc
               :x-pos (op (:x-pos @current-pos) x-diff)
               :y-pos (op (:y-pos @current-pos) y-diff))))
    @anti-nodes))

(defn generate-anti-nodes
  "docstring"
  [left right board till-out-of-bounds]
  (let [x-diff (- (:x-pos left) (:x-pos right))
        y-diff (- (:y-pos left) (:y-pos right))
        new-left-position {:x-pos (+ (:x-pos left) x-diff) :y-pos (+ (:y-pos left) y-diff)}
        new-right-position {:x-pos (- (:x-pos right) x-diff) :y-pos (- (:y-pos right) y-diff)}]
    (if till-out-of-bounds
      (concat
        (generate-anti-nodes-till-out-of-bounds (:x-pos left) (:y-pos left) x-diff y-diff board +)
        (generate-anti-nodes-till-out-of-bounds (:x-pos right) (:y-pos right) x-diff y-diff board -))
      [new-left-position new-right-position])))


(defn generate-all-anti-nodes
  "docstring"
  [positions board till-end-of-board]
  (loop [elem1 (first positions)
         xs (rest positions)
         acc []]
    (let [combinations (when (not-empty xs) (map (fn [elem] [elem1 elem]) xs))]
      (if (empty? xs)
        (flatten acc)
        (recur (first xs)
               (rest xs)
               (into acc (map #(generate-anti-nodes (first %) (second %) board till-end-of-board) combinations)))))))

(defn solve-part-1
  "docstring"
  [filename]
  (let [raw-lines (io/read-input filename)
        board (board/parse-to-board raw-lines)
        all-distinct-symbols (find-all-distinct-symbols board)
        all-anti-nodes (->> all-distinct-symbols
                            (keys)
                            (map #(generate-all-anti-nodes (get all-distinct-symbols %) board false))
                            flatten
                            set)
        filtered (filter #(board/is-on-board? (:x-pos %) (:y-pos %) board) all-anti-nodes)
        on-board-count (count filtered)
        ]
    on-board-count))


(defn solve-part-2
  "docstring"
  [filename]
  (let [raw-lines (io/read-input filename)
        board (board/parse-to-board raw-lines)
        all-distinct-symbols (find-all-distinct-symbols board)
        all-anti-nodes (->> all-distinct-symbols
                            (keys)
                            (map #(generate-all-anti-nodes (get all-distinct-symbols %) board true))
                            flatten
                            set)
        filtered (filter #(board/is-on-board? (:x-pos %) (:y-pos %) board) all-anti-nodes)
        values-flat (map #(dissoc % :symbol) (flatten (vals all-distinct-symbols)))
        dedup (->
                (reduce (fn [coll item] (conj coll item)) filtered values-flat)
                set)
        ]
    (count dedup)))

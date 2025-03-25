(ns advent-of-code-2024.week2.day12
  (:require [advent-of-code-2024.utils.io :as io]
            [advent-of-code-2024.utils.board :as board]
            [clojure.pprint :as pp]
            [clojure.set :as set]
            [advent-of-code-2024.utils.algorithms :as algo]))

(defn get-all-different-symbols
  "Get all the different symbols found on board.
  Returns a set of different symbols."
  [board]
  (set (:board board)))

(defn get-all-positions-for-symbol
  "Return all the positions on a board for a given symbol."
  [board symbol]
  (for [x-range (range (:width board))
        y-range (range (:height board))
        :when (= symbol (board/get-pos x-range y-range board))
        ]
    {:x-pos x-range :y-pos y-range}))

(defn determine-all-regions-for-symbol
  "Determine all the regions for a given symbol."
  [board symbol]
  (loop [remaining-positions (set (get-all-positions-for-symbol board symbol))
         acc-regions []
         ]
    (if (empty? remaining-positions)
      acc-regions
      (let [mapped-region (algo/flood-fill board (first remaining-positions))
            new-remaining-positions (set/difference remaining-positions mapped-region)
            ]
        (recur new-remaining-positions (conj acc-regions mapped-region)))
      )))

(defn map-all-symbols-to-regions
  [board]
  (->> (reduce (fn [coll symbol]
            (conj coll [symbol (determine-all-regions-for-symbol board symbol)]))
          []
          (get-all-different-symbols board))
       (into {})))


(defn is-position-part-of-edge?
  "Determine if given a board and a position, return true if position is an edge.
  Otherwise, return false."
  [board position]
  (let [neighbours [(board/is-same-symbol-left? board position)
                    (board/is-same-symbol-right? board position)
                    (board/is-same-symbol-top? board position)
                    (board/is-same-symbol-bottom? board position)]
        ]
    (io/in? false neighbours)))

(defn get-all-edges-for-region
  "Given a board and a region (set of all the positions), we return the edges of
  that region. The edges are returned in a set, as duplicates do not add extra
  meaning."
  [board region]
  (reduce (fn [coll item]
            (if (true? (is-position-part-of-edge? board item))
              (conj coll item)
              coll))
          #{}
          region))

(defn get-all-edges-for-regions
  "docstring"
  [board regions]
  (map #(get-all-edges-for-region board %) regions))

(defn get-perimeter-for-region
  ""
  [x-poss->positions]
  0)

(defn get-area-of-region
  [x-poss->positions]
  (count x-poss->positions))

(defn is-region-within-another-region
  [region-a region-b]
  false)

(defn solve-part-1
  [filename]
  (let [raw-lines (io/read-input "day12/example.txt")
        board (board/parse-to-board raw-lines)
        _ (board/print-board board)
        all-board-symbols (get-all-different-symbols board)
        ;_ (println all-board-symbols)
        ;_ (println (get-all-positions-for-symbol board (first all-board-symbols)))
        ;; rs (flood-fill board {:x-pos 9 :y-pos 0})
        ;; rs (determine-all-regions-for-symbol board \I)
        ;; _ (println "all the R's at 0,0: ")
        ;; _ (pp/pprint rs)
        whole-map (map-all-symbols-to-regions board)
        _ (pp/pprint whole-map)
        rs (get whole-map \C)
        ;; _ (pp/pprint rs)
        edges-for-r (get-all-edges-for-regions board rs)
        _ (pp/pprint edges-for-r)
        ]
    )
  0)

(defn solve-part-2
  ""
  [filename]
  )


;; BIG example
;; It contains:

;; A region of R plants with price 12 * 18 = 216.
;; A region of I plants with price 4 * 8 = 32.
;; A region of C plants with price 14 * 28 = 392.
;; A region of F plants with price 10 * 18 = 180.
;; A region of V plants with price 13 * 20 = 260.
;; A region of J plants with price 11 * 20 = 220.
;; A region of C plants with price 1 * 4 = 4.
;; A region of E plants with price 13 * 18 = 234.
;; A region of I plants with price 14 * 22 = 308.
;; A region of M plants with price 5 * 12 = 60.
;; A region of S plants with price 3 * 8 = 24.

;; So, it has a total price of 1930.

(ns advent-of-code-2024.week2.day12
  (:require [advent-of-code-2024.utils.io :as io]
            [advent-of-code-2024.utils.board :as board]
            [clojure.pprint :as pp])
  )

(def not-contains? (complement contains?))

(defn next-flood-fill-steps
  "Determine the nex possible steps for the flood-fill algorithm"
  [board curr-pos seen]
  (let [left-pos  (-> (board/is-same-symbol-left? board curr-pos)
                      (when (board/get-pos-left curr-pos)))
        right-pos (-> (board/is-same-symbol-right? board curr-pos)
                      (when (board/get-pos-right curr-pos)))
        top-pos (-> (board/is-same-symbol-top? board curr-pos)
                    (when (board/get-pos-top curr-pos)))
        bottom-pos (-> (board/is-same-symbol-bottom? board curr-pos)
                       (when (board/get-pos-bottom curr-pos)))
        ]
    (->> [left-pos right-pos top-pos bottom-pos]
         (filter some?)
         (filter #(not-contains? seen %))))
  )

(defn flood-fill
  "Run the flood-fill algorithm given a board and a starting position"
  ([board start-pos] (flood-fill board [start-pos] #{}))
  ([board start-pos seen]
   (loop [currently-seen seen
          current-to-visit start-pos
          ]
     (if (empty? current-to-visit)
       currently-seen
       (let [next-steps (next-flood-fill-steps board (first current-to-visit) currently-seen)
             updated-seen (-> currently-seen
                              (conj (first current-to-visit))
                              (into next-steps))
             _ (println "updated seen:" updated-seen)
             ]
         (recur updated-seen (into (rest current-to-visit) next-steps)))))
   )
  )

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
  [])

(defn get-vertial-perimeter-for-region
  ""
  []
  0)

(defn get-vertical-perimeter-for-region
  ""
  []
  0)

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
        rs (flood-fill board {:x-pos 4 :y-pos 0})
        _ (println "all the R's at 0,0: ")
        _ (pp/pprint rs)
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

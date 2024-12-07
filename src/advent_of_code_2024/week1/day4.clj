(ns advent-of-code-2024.week1.day4
  (:require [advent-of-code-2024.utils.board :as board]
            [advent-of-code-2024.utils.io :as io]))

(defn parse-to-board
  [raw-lines]
  (let [flattened-data (->>
                         (map #(vec (char-array %1)) raw-lines)
                         (flatten)
                         (vec))
        width (count (first raw-lines))
        height (count raw-lines)
        empty-board (board/new width height)
        ]
    (board/update-board-data empty-board flattened-data)))

(defn is-target?
  [target suspect]
  (= target suspect))

(defn can-get-target-at-position?
  [xPos yPos board target]
  (let [target-length (count target)
        backwards (board/get-data-left xPos yPos board target-length)
        forwards (board/get-data-right xPos yPos board target-length)
        top (board/get-data-top xPos yPos board target-length)
        bottom (board/get-data-bottom xPos yPos board target-length)

        top-left (board/get-data-top-left xPos yPos board target-length)
        top-right (board/get-data-top-right xPos yPos board target-length)
        bottom-left (board/get-data-bottom-left xPos yPos board target-length)
        bottom-right (board/get-data-bottom-right xPos yPos board target-length)

        all-values [backwards forwards top bottom top-left top-right bottom-left bottom-right]
        all-values-str (map #(apply str %) all-values)
        ]
    (count (filter true? (map #(is-target? % target) all-values-str)))))


(defn solve-part-1
  [filename]
  (let [raw-lines (io/read-input filename)
        filled-board (parse-to-board raw-lines)
        found-target-count (for [yRange (range (:height filled-board))
                                 xRange (range (:width filled-board))]
                             (can-get-target-at-position? xRange yRange filled-board "XMAS"))]
    (reduce + found-target-count)))


(defn is-target-2?
  [target suspect]
  (or (= target suspect)
      (= (clojure.string/reverse target) suspect)))

(defn can-get-target-at-position-2?
  [xPos yPos board target]
  (let [target-length (count target)
        bottom-left (board/get-data-bottom-left (+ xPos 2) yPos board target-length)
        bottom-right (board/get-data-bottom-right xPos yPos board target-length)
        all-values [bottom-left bottom-right]
        all-values-str (map #(apply str %) all-values)
        left-is-mas (is-target-2? target (first all-values-str))
        right-is-mas (is-target-2? target (second all-values-str))
        ]
    (if (and left-is-mas right-is-mas)
      1
      0)))

(defn solve-part-2
  [filename]
  (let [raw-lines (io/read-input filename)
        filled-board (parse-to-board raw-lines)
        found-target-count (for [xRange (range (:width filled-board))
                                 yRange (range (:height filled-board))]
                             (can-get-target-at-position-2? xRange yRange filled-board "MAS"))
        ]
    (reduce + found-target-count)))
(ns advent-of-code-2024.week3.day15
  (:require [advent-of-code-2024.utils.io :as io]
            [advent-of-code-2024.utils.board :as b]
            [clojure.pprint :as pp]))

(defn parse-raw-lines-to-board
  [raw-lines]
  (let [raw-board-lines (filter #(clojure.string/starts-with? % "#") raw-lines)
        board (b/parse-to-board raw-board-lines)
        ]
    board))

(def starter \@)
(defn find-starting-position
  [board]
  (for [x-pos (range (:width board))
        y-pos (range (:height board))
        :when (= starter (b/get-pos x-pos y-pos board))
        ]
    [x-pos y-pos]))

(defn parse-raw-lines-to-instructions
  [raw-lines]
  (let [raw-instructions (drop-while #(or (clojure.string/starts-with? % "#")
                                          (clojure.string/blank? %))
                                     raw-lines)]
    (clojure.string/join raw-instructions)))

(defn get-direction-string
  [board curr-pos direction]
  (let [x-pos (first curr-pos)
        y-pos (second curr-pos)
        str (cond
              (= \> direction) (b/get-data-right (inc x-pos) y-pos board (- (:width board) (inc x-pos)))
              (= \< direction) (b/get-data-left (dec x-pos) y-pos board x-pos)
              (= \^ direction) (b/get-data-top (x-pos) (dec y-pos) board y-pos)
              (= \v direction) (b/get-data-bottom x-pos (inc y-pos) board (- (:height board) (inc y-pos)))
              )
        ]
    str) ;; return empty vector for now
  )


;; TODO We need to decide when are on the edge. We cannot inc or dec beyond the edge.
(defn move
  [board curr-pos direction]
  (let [;; Get the current string
        ]
    (cond
      (= \> direction) "go right"
      (= \< direction) "go left"
      (= \^ direction) "go up"
      (= \v direction) "go down"
      :else "unknown")
    ))

(defn compress-way
  ""
  [str direction]
  "Implement me")

(defn update-board
  ""
  [curr-pos direction board]
  "implement me")

(def empty-space \.)

(defn can-move?
  [line]
  (boolean (some #(= % empty-space) line)))

(can-move? [1 2 3 4 5 \x])
(can-move? [1 2 3 4 5 \. \x])

;(defn solve-part-1
;  ""
;  [filename]
;  (let [raw-lines (io/read-input "day15/example.txt")
;        board (parse-raw-lines-to-board raw-lines)
;        start-pos (first (find-starting-position board))
;        _ (b/print-board board)
;        _ (println start-pos)
;        instructions (parse-raw-lines-to-instructions raw-lines)
;        _ (println "move instructions: " instructions (count instructions))
;        str-left
;        ]
;    0)
;  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try to implement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

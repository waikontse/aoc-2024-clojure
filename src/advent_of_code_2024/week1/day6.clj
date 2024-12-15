(ns advent-of-code-2024.week1.day6
  (:require [advent-of-code-2024.utils.io :as io])
  (:require [advent-of-code-2024.utils.board :as board]))

(def obstacle \#)
(def marked \X)
(def starter \^)

(defn find-starter
  [board starter-symbol]
  (let [found-positions (for [x (range (:width board))
                              y (range (:height board))]
                          (when (= starter-symbol (board/get-pos x y board))
                            {:x-pos x :y-pos y}))]
    (->> found-positions
         (filter map?)
         (first))))

(defn determine-next-position
  [current-position direction]
  (let [x-position (:x-pos current-position)
        y-position (:y-pos current-position)
        new-x-pos (cond
                    (= :WEST direction) (dec x-position)
                    (= :EAST direction) (inc x-position)
                    :else (:x-pos current-position))
        new-y-pos (cond
                    (= :NORTH direction) (dec y-position)
                    (= :SOUTH direction) (inc y-position)
                    :else (:y-pos current-position))]
    {:x-pos new-x-pos :y-pos new-y-pos}))

(defn is-obstacle?
  [board current-position marker]
  (if (= (board/get-pos (:x-pos current-position) (:y-pos current-position) board) marker)
    true
    false))

(defn can-move?
  [current-position board direction]
  (let [new-position (determine-next-position current-position direction)
        is-off-board (board/is-off-board? (:x-pos new-position) (:y-pos new-position) board)
        is-obstacle (is-obstacle? board new-position obstacle)
        ;_ (println "can-move?: " new-position direction "is-off-board: "is-off-board "is-obstacle" is-obstacle)
        ]
    (cond
      (true? is-off-board) false
      (true? is-obstacle) false
      :else true)))

(defn get-next-direction-if-blocked
  [current-direction]
  (cond
    (= current-direction :NORTH) :EAST
    (= current-direction :EAST) :SOUTH
    (= current-direction :SOUTH) :WEST
    (= current-direction :WEST) :NORTH
    :else (ex-info "this is unexpected" {})))

(defn move-and-mark
  [new-position board marked]
  (board/set-pos (:x-pos new-position) (:y-pos new-position) board marked))

(defn has-looped?
  [visited-positions current-position direction]
  (contains? visited-positions (conj current-position {:direction direction})))

(defn make-moves-until-stopped
  [start-position board]
  (loop [current-position start-position
         direction :NORTH
         current-board board
         visited #{}]
    (let [
          _ (println "makinng a move: " current-position direction)
          can-move (can-move? current-position current-board direction)
          ;is-off-board (board/is-off-board? (:x-pos current-position) (:y-pos current-position) current-board)
          current-next-position (determine-next-position current-position direction)
          is-blocked (is-obstacle? current-board current-next-position obstacle)
          newly-marked-board (move-and-mark current-position current-board marked)
          ;_ (println "bools: " can-move is-off-board is-blocked)
          has-looped (has-looped? visited current-position direction)
          ]
      (cond
        (true? has-looped) 1
        (true? can-move) (recur (determine-next-position current-position direction)
                                direction
                                newly-marked-board
                                (conj visited (conj current-position {:direction direction})))
        (true? is-blocked) (recur (determine-next-position current-position (get-next-direction-if-blocked direction))
                                  (get-next-direction-if-blocked direction)
                                  newly-marked-board
                                  (conj visited (conj current-position {:direction direction})))
        :else newly-marked-board)
      )
    )
  )

(defn solve-part-1
  "docstring"
  [filename]
  (let [raw-lines (io/read-input filename)
        board (board/parse-to-board raw-lines)
        _ (println board)
        start-pos (find-starter board starter)
        _ (println "starting pos: " start-pos)
        new-board-data (make-moves-until-stopped start-pos board)
        total-marked (count (filter #(= marked %) (:board new-board-data)))
        ]
   total-marked))

(defn solve-part-2
  "docstring"
  [filename]
  (let [raw-lines (io/read-input filename)
        board (board/parse-to-board raw-lines)
        ;_ (println board)
        start-pos (find-starter board starter)
        new-board-data (make-moves-until-stopped start-pos board)
        _ (println new-board-data)
        ]
    0))
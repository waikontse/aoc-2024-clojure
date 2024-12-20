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
        is-obstacle (is-obstacle? board new-position obstacle)]
    (cond
      (true? is-off-board) false
      (true? is-obstacle) false
      :else true)))

(defn get-next-direction-if-blocked
  [current-direction current-position board]
  (let [next-direction (cond
                         (= current-direction :NORTH) :EAST
                         (= current-direction :EAST) :SOUTH
                         (= current-direction :SOUTH) :WEST
                         (= current-direction :WEST) :NORTH
                         :else (ex-info "this is unexpected" {}))
        next-position (determine-next-position current-position next-direction)
        is-blocked (is-obstacle? board next-position obstacle)]
    (if (false? is-blocked)
      next-direction
      (get-next-direction-if-blocked next-direction current-position board))
    )
  )

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
    (let [can-move (can-move? current-position current-board direction)
          current-next-position (determine-next-position current-position direction)
          is-blocked (is-obstacle? current-board current-next-position obstacle)
          newly-marked-board (move-and-mark current-position current-board marked)
          has-looped (has-looped? visited current-position direction)]
      (cond
        (true? has-looped) 1
        (true? can-move) (recur (determine-next-position current-position direction)
                                direction
                                newly-marked-board
                                (conj visited (conj current-position {:direction direction})))
        (true? is-blocked) (recur (determine-next-position current-position
                                                           (get-next-direction-if-blocked direction current-position board))
                                  (get-next-direction-if-blocked direction current-position board)
                                  newly-marked-board
                                  (conj visited (conj current-position {:direction direction})))
        :else newly-marked-board))))

(defn solve-part-1
  [filename]
  (let [raw-lines (io/read-input filename)
        board (board/parse-to-board raw-lines)
        start-pos (find-starter board starter)
        new-board-data (make-moves-until-stopped start-pos board)
        total-marked (count (filter #(= marked %) (:board new-board-data)))]
   total-marked))

(defn generate-obstacles
  "Generate a sequence of position where a new obstacle can be placed"
  [board starting-position]
  (let [positions-for-obstacle (for [x-range (range (:width board))
                                     y-range (range (:height board))]
                                 (when (not= {:x-pos x-range :y-pos y-range} starting-position)
                                   {:x-pos x-range :y-pos y-range}))
        ]
    (filter some? positions-for-obstacle)))

(defn solve-part-2
  [filename]
  (let [raw-lines (io/read-input filename)
        board (board/parse-to-board raw-lines)
        start-pos (find-starter board starter)
        obstacle-locations (generate-obstacles board start-pos)
        new-board-data (->> (map #(make-moves-until-stopped start-pos (move-and-mark % board obstacle)) obstacle-locations)
                            (filter number?))]
    (count new-board-data)))

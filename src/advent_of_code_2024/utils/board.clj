(ns advent-of-code-2024.utils.board)

;; DESCRIPTION
;;     Datastructure description
;;     {
;;         :width  <int>
;;         :height <int>
;;         :board  <Array<T>>
;;     }

(defn new
  "Create a new board of width X and height Y"
  [x y]
  {:width x :height y :board []})

(defn is-within-x-range?
  [xPos board]
  (and (>= xPos 0)
       (< xPos (:width board))))

(defn is-within-y-range?
  "docstring"
  [yPos board]
  (and (>= yPos 0)
       (< yPos (:height board))))

(defn is-off-board?
  "docstring"
  [xPos yPos board]
  (cond
    (< xPos 0) true
    (< yPos 0) true
    (>= xPos (:width board)) true
    (>= yPos (:height board)) true
    :else false))

(defn is-on-board?
  "docstring"
  [x-pos y-pos board]
  (not (is-off-board? x-pos y-pos board)))

(defn get-internal-position
  "Calculate the internal position of the board, given X and Y position"
  [xPos yPos board]
  (+ xPos (* yPos (:width board))))

(defn get-pos
  "Get the item at position X and position Y"
  [xPos yPos board]
  (let [board-data (:board board)]
    (if (and (is-within-x-range? xPos board)
             (is-within-y-range? yPos board))
      (get board-data (get-internal-position xPos yPos board))
      nil)))

(defn update-board-data
  "docstring"
  [old-board new-board-data]
  {:width (:width old-board) :height (:height old-board) :board new-board-data})

(defn parse-to-board
  [raw-lines]
  (let [flattened-data (->>
                        (map #(vec (seq %1)) raw-lines)
                        (flatten)
                        (vec))
        width (count (first raw-lines))
        height (count raw-lines)
        empty-board (advent-of-code-2024.utils.board/new width height)]
    (update-board-data empty-board flattened-data)))

(defn set-pos
  "Set the item at position X and position Y into the *new* board"
  [xPos yPos board item]
  (let [updated-board-value (assoc
                             (:board board)
                             (get-internal-position xPos yPos board) item)]
    (update-board-data board updated-board-value)))

(defn can-get-data-left?
  "docstring"
  [xPos board length]
  (and (>= (- xPos (dec length)) 0)
       (is-within-x-range? xPos board)))

(defn get-data-left
  "docstring"
  [xPos yPos board length]
  (let [fetched-values (mapv #(get-pos (- xPos %) yPos board) (range length))]
    fetched-values))

(defn can-get-data-right?
  [xPos board length]
  (and (< (+ xPos (dec length)) (:width board))
       (is-within-x-range? xPos board)))

(defn get-data-right
  "docstring"
  [xPos yPos board length]
  (let [fetched-values (mapv #(get-pos (+ xPos %) yPos board) (range length))]
    fetched-values))

(defn can-get-data-top?
  [yPos board length]
  (and (>= (- yPos (dec length)) 0)
       (is-within-y-range? yPos board)))

(defn get-data-top
  "docstring"
  [xPos yPos board length]
  (let [fetched-values (mapv #(get-pos xPos (- yPos %) board) (range length))]
    fetched-values))

(defn can-get-data-bottom?
  [yPos board length]
  (and (< (+ yPos (dec length)) (:height board))
       (is-within-y-range? yPos board)))

(defn get-data-bottom
  "docstring"
  [xPos yPos board length]
  (let [fetched-values (mapv #(get-pos xPos (+ yPos %) board) (range length))]
    fetched-values))

(defn can-get-data-top-left
  [xPos yPos board length]
  (and (can-get-data-top? yPos board length)
       (can-get-data-left? xPos board length)))

(defn get-data-top-left
  "docstring"
  [xPos yPos board length]
  (let [fetched-values (mapv #(get-pos (- xPos %) (- yPos %) board) (range length))]
    fetched-values))

(defn can-get-data-top-right
  [xPos yPos board length]
  (and (can-get-data-top? yPos board length)
       (can-get-data-right? xPos board length)))

(defn get-data-top-right
  "docstring"
  [xPos yPos board length]
  (let [fetched-values (mapv #(get-pos (+ xPos %) (- yPos %) board) (range length))]
    fetched-values))

(defn can-get-data-bottom-left?
  [xPos yPos board length]
  (and (can-get-data-bottom? yPos board length)
       (can-get-data-left? xPos board length)))

(defn get-data-bottom-left
  "docstring"
  [xPos yPos board length]
  (let [fetched-values (mapv #(get-pos (- xPos %) (+ yPos %) board) (range length))]
    fetched-values))

(defn can-get-data-bottom-right?
  [xPos yPos board length]
  (and (can-get-data-bottom? yPos board length)
       (can-get-data-right? xPos board length)))

(defn get-data-bottom-right
  "docstring"
  [xPos yPos board length]
  (let [fetched-values (mapv #(get-pos (+ xPos %) (+ yPos %) board) (range length))]
    fetched-values))

;; Convert current position to another adjacent position
(defn get-pos-top
  [pos]
  {:x-pos (:x-pos pos) :y-pos (dec (:y-pos pos))})

(defn get-pos-bottom
  [pos]
  {:x-pos (:x-pos pos) :y-pos (inc (:y-pos pos))})

(defn get-pos-left
  [pos]
  {:x-pos (dec (:x-pos pos)) :y-pos (:y-pos pos)})

(defn get-pos-right
  [pos]
  {:x-pos (inc (:x-pos pos)) :y-pos (:y-pos pos)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding / searching in the board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn find-all-chars-in-board
  [board target]
  (let [width (:width board)
        height (:height board)]
    (for [x (range 0 width)
          y (range 0 height)
          :let [currPos (get-pos x y board)]
          :when (= target currPos)]
      [x y])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testing board properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn is-same-symbol?
  [board curr-pos can-get-next? f-next]
  (if can-get-next?
    (let [symbol (get-pos (:x-pos curr-pos) (:y-pos curr-pos) board)
          next-symbol (f-next curr-pos)]
      (= symbol next-symbol))
    false))

(defn is-same-symbol-left?
  [board curr-pos]
  (is-same-symbol?
   board curr-pos
   (can-get-data-left? (:x-pos curr-pos) board 2)
   #(last (get-data-left (:x-pos %) (:y-pos %) board 2))))

(defn is-same-symbol-right?
  [board curr-pos]
  (is-same-symbol?
   board curr-pos
   (can-get-data-right? (:x-pos curr-pos) board 2)
   #(last (get-data-right (:x-pos %) (:y-pos %) board 2))))

(defn is-same-symbol-top?
  [board curr-pos]
  (is-same-symbol?
   board curr-pos
   (can-get-data-top? (:y-pos curr-pos) board 2)
   #(last (get-data-top (:x-pos %) (:y-pos %) board 2))))

(defn is-same-symbol-bottom?
  [board curr-pos]
  (is-same-symbol?
   board curr-pos
   (can-get-data-bottom? (:y-pos curr-pos) board 2)
   #(last (get-data-bottom (:x-pos %) (:y-pos %) board 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diagnostics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn print-board
  [board]
  (let [rows (partition (:width board) (:board board))]
    (printf "Board width: %d height: %d%n" (:width board) (:height board))
    (dorun (map #(println (apply str %)) rows))))

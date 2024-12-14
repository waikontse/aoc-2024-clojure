(ns advent-of-code-2024.utils.board)

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

(defn set-pos
  "Set the item at position X and position Y into the *new* board"
  [xPos yPos board item]
  (let [updated-board-value (assoc
                              (:board board)
                              (get-internal-position xPos yPos board) item)]
    (update-board-data board updated-board-value)))

(defn parse-to-board
  [raw-lines]
  (let [flattened-data (->>
                         (map #(vec (char-array %1)) raw-lines)
                         (flatten)
                         (vec))
        width (count (first raw-lines))
        height (count raw-lines)
        empty-board (advent-of-code-2024.utils.board/new width height)
        ]
    (update-board-data empty-board flattened-data)))

(defn can-get-data-left?
  "docstring"
  [xPos board length]
  (and (>= (- xPos length) 0)
       (is-within-x-range? xPos board)))

(defn get-data-left
  "docstring"
  [xPos yPos board length]
  (let [fetched-values (mapv #(get-pos (- xPos %) yPos board) (range length))]
    fetched-values))


(defn can-get-data-right?
  [xPos board length]
  (and (< (+ xPos length) (:width board))
       (is-within-x-range? xPos board)))

(defn get-data-right
  "docstring"
  [xPos yPos board length]
  (let [fetched-values (mapv #(get-pos (+ xPos %) yPos board) (range length))]
    fetched-values))


(defn can-get-data-top?
  [yPos board length]
  (and (>= (- yPos length) 0)
       (is-within-y-range? yPos board)))

(defn get-data-top
  "docstring"
  [xPos yPos board length]
  (let [fetched-values (mapv #(get-pos xPos (- yPos %) board) (range length))]
    fetched-values))


(defn can-get-data-bottom?
  [yPos board length]
  (and (< (+ yPos length) (:height board))
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
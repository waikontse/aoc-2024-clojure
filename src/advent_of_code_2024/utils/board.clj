(ns advent-of-code-2024.utils.board)

(defn new
  "Create a new board of width X and height Y"
  [x y]
  {:width x :height y :board []})

(defn get-internal-position
  "Calculate the internal position of the board, given X and Y position"
  [xPos yPos board]
  (+ xPos (* yPos (:width board))))

(defn get-pos
  "Get the item at position X and position Y"
  [xPos yPos board]
  (let [board-data (:board board)]
    (get board-data (get-internal-position xPos yPos board))))

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

(defn get-data-back
  "docstring"
  [xPos yPos board length]
  nil)

(defn get-data-front
  "docstring"
  [xPos yPos board lenght]
  nil)

(defn get-data-top
  "docstring"
  [xPos yPos board length]
  nil)

(defn get-data-bottom
  "docstring"
  [xPos yPos board length]
  nil)

(defn get-data-top-left
  "docstring"
  [xPos yPos board length]
  nil)

(defn get-data-top-right
  "docstring"
  [xPos yPos board length]
  nil)

(defn get-data-bottom-left
  "docstring"
  [xPos yPos board length]
  nil)

(defn get-data-bottom-right
  "docstring"
  [xPos yPos board length]
  nil)
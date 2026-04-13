(ns advent-of-code-2025.week1.day7
  (:require [utils.board :as board]))

(def example (slurp "./resources/y2025/day7/example.txt"))
(def input (slurp "./resources/y2025/day7/input.txt"))
(def start \S)
(def splitter \^)
(def laser \|)
(def empty-char \.)

(defn is-splitter?
  [board [x y]]
  (let [char (board/get-pos x y board)]
    (= splitter char)))

(defn can-mark?
  [board [x y]]
  (let [is-inside-board? (board/is-on-board? x y board)
        curr-pos (board/get-pos x y board)
        is-empty-char? (= curr-pos empty-char)
        ]
    (and is-inside-board? is-empty-char?)))

(defn is-already-marked?
  [board [x y]]
  (let [curr-pos (board/get-pos x y board)]
    (= laser curr-pos)))

(defn mark
  [board [x y]]
  (board/set-pos x y board laser))

(defn move-down
  [[x y]]
  [x (inc y)])

(defn move-left
  [[x y]]
  [(dec x) y])

(defn move-right
  [[x y]]
  [(inc x) y])


(defn shoot-laser
  [board [x y] atom-for-tachyons]
  (loop [updated-board board
         positions [[x y]]
        ]
    (let [curr-pos (first positions)
          [x y] curr-pos]
      (cond
        (empty? positions) updated-board
        (board/is-off-board? x y updated-board) (recur updated-board (rest positions))
        (is-already-marked? updated-board curr-pos) (recur updated-board (rest positions))
        (can-mark? updated-board curr-pos) (recur
                                             (mark updated-board curr-pos)
                                             (-> (rest positions)
                                                 (vec)
                                                 (conj (move-down curr-pos))))
        (is-splitter? updated-board curr-pos) (do
                                                (swap! atom-for-tachyons inc)
                                                (recur
                                                  updated-board
                                                  (-> (rest positions)
                                                      (vec)
                                                      (conj (move-left curr-pos))
                                                      (conj (move-right curr-pos)))))
        :else (throw (Exception. "unsupported branch"))))))


(defn solve-part-1
  [input]
  (let [raw-lines (clojure.string/split-lines input)
        parsed-board (board/parse-to-board raw-lines)
        starting-pos (first (board/find-all-chars-in-board parsed-board start))
        atom-for-tachyons (atom 0)
        _ (shoot-laser parsed-board (move-down starting-pos) atom-for-tachyons)
        ]
    (deref atom-for-tachyons)))

;; 1658
(solve-part-1 input)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def mem-shoot-laser-recur
  (memoize (fn [board [x y]]
             (cond
               (board/is-off-board? x y board) 1
               (can-mark? board [x y]) (mem-shoot-laser-recur board (move-down [x y]))
               (is-splitter? board [x y]) (+ (mem-shoot-laser-recur board (move-left [x y]))
                                             (mem-shoot-laser-recur board (move-right [x y])))
               :else (throw (Exception. "unsupported branch"))
             ))))

(defn solve-part-2
  [input]
  (let [raw-lines (clojure.string/split-lines input)
        parsed-board (board/parse-to-board raw-lines)
        starting-pos (first (board/find-all-chars-in-board parsed-board start))
        parallel-universes (mem-shoot-laser-recur parsed-board (move-down starting-pos))
        _ (println "parallel universes:" parallel-universes)
        ]
    parallel-universes))

(solve-part-2 input)
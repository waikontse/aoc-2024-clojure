(ns advent-of-code-2025.week1.day7
  (:require [advent-of-code-2024.utils.board :as board]))

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

(defn find-all-marked-tachyons
  [board tachyons-pos]
  (let [updated-tachyons-pos (map (fn [[x y]] [x (dec y)]) tachyons-pos)
        positions (map (fn [[x y]] (board/get-pos x y board)) updated-tachyons-pos)
        filtered (filter #(= laser %) positions)
        ]
    (count filtered))
  )

(defn shoot-laser
  [board [x y] atom-for-tachyons atom-for-finishes]
  (loop [updated-board board
         positions [[x y]]
        ]
    (let [curr-pos (first positions)
          [x y] curr-pos]
      (cond
        (empty? positions) updated-board
        (board/is-off-board? x y updated-board) (do
                                                  (swap! atom-for-finishes inc)
                                                  (recur updated-board (rest positions)))
        (is-already-marked? updated-board curr-pos) (recur
                                                      updated-board
                                                      (-> (rest positions)
                                                          (vec)
                                                          (conj (move-down curr-pos))))
                                                   ;; (recur updated-board (rest positions))
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
        :else (do
                (println "curr pos:" x y)
                (println (board/get-pos x y updated-board))
                (throw (Exception. "unsupported branch")))
        )
      )
    )
  )

(defn solve-part-1
  [input]
  (let [raw-lines (clojure.string/split-lines input)
        parsed-board (board/parse-to-board raw-lines)
        starting-pos (first (board/find-all-chars-in-board parsed-board start))
        tachyons (board/find-all-chars-in-board parsed-board splitter)
        ;updated-board (shoot-laser parsed-board [(first starting-pos) (inc (second starting-pos))])
        atom-for-tachyons (atom 0)
        atom-for-finishes (atom 0)
        updated-board (shoot-laser parsed-board (move-down starting-pos) atom-for-tachyons atom-for-finishes)
        ;activated-tachyons (find-all-marked-tachyons updated-board tachyons)
        _ (println "starting pos:" starting-pos)
        _ (println "tachyons:" tachyons)
        _ (println "atom for tachyons" atom-for-tachyons)
        _ (println "atom for finished" atom-for-finishes)
        _ (print "filter activated tachyons:" (:val atom-for-tachyons))
        ]
    (:val atom-for-tachyons))
  )

;; 1658
(solve-part-1 example)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(def count-atom (atom 0))
;(swap! count-atom inc)
(defn shoot-laser-recur
  [board [x y] atom-for-tachyons]
  (cond
    (board/is-off-board? x y board) 1
    (can-mark? board [x y]) (shoot-laser-recur board (move-down [x y]) atom-for-tachyons)
    (is-splitter? board [x y]) (do
                                 (swap! atom-for-tachyons inc)
                                 (+ (shoot-laser-recur board (move-left [x y]) atom-for-tachyons)
                                    (shoot-laser-recur board (move-right [x y]) atom-for-tachyons)))
    :else (do
            (println "curr pos:" x y)
            (println (board/get-pos x y board))
            (throw (Exception. "unsupported branch")))
    )
  )



(defn solve-part-2
  [input]
  (let [raw-lines (clojure.string/split-lines input)
        parsed-board (board/parse-to-board raw-lines)
        starting-pos (first (board/find-all-chars-in-board parsed-board start))
        atom-for-tachyons (atom 0)
        parallel-universes (shoot-laser-recur parsed-board (move-down starting-pos) atom-for-tachyons)
        _ (println "starting pos:" starting-pos)
        _ (println "atom for tachyons" atom-for-tachyons)
        _ (println "parallel universes:" parallel-universes)
        ]
    (:val atom-for-tachyons))
  )

(solve-part-2 input)
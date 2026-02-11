(ns advent-of-code-2024.utils.algorithms
  (:require [advent-of-code-2024.utils.board :as board]
            [clojure.math])
  )

(def not-contains? (complement contains?))
(def EPS 1e-9)

(defn next-flood-fill-steps
  "Determine the nex possible steps for the flood-fill algorithm"
  [board curr-pos seen]
  (let [left-pos (-> (board/is-same-symbol-left? board curr-pos)
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
  "Try to run a flood-fill algorithm given a board and a starting position.
    The algorithm will use the same value as the one found on th board in the
    position.

  Returns a set of position(s) "
  ([board start-pos] (flood-fill board [start-pos] #{}))
  ([board start-pos seen]
   (loop [currently-seen seen
          current-to-visit start-pos
          ]
     (if (empty? current-to-visit)
       currently-seen
       (let [next-steps (next-flood-fill-steps board (first current-to-visit) currently-seen)
             updated-seen (conj currently-seen (first current-to-visit))
             ;; _ (println "updated seen:" updated-seen)
             ]
         (recur updated-seen (into (rest current-to-visit) next-steps)))))
   ))

(defn manhattan
  "Compute the manhattan distance between 2 points"
  [from to]
  (let [x-diff (abs (- (:x-pos from) (:x-pos to)))
        y-diff (abs (- (:y-pos from) (:y-pos to)))
        ]
    (+ x-diff y-diff)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START OF linear algebra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn row-divide-by
  [row divisor]
  (map #(/ % divisor) row))

(defn row-multiply-by
  [row multiplier]
  (map #(* % multiplier) row))

(defn row-subtract
  [m1 m2]
  (->> (map vector m1 m2)
       (map #(apply - %))))

(defn row-reduce
  [reduced toBeReduced col]
  (let [multiplier (nth toBeReduced col)
        multipliedReduced (row-multiply-by reduced multiplier)]
    (row-subtract toBeReduced multipliedReduced)))

(defn row-simplify-col
  [row col]
  (let [divisor (nth row col)]
    (row-divide-by row divisor)))

(defn matrix-reduce-col
  [matrix col]
  (let [before (flatten (take col matrix))
        reduced (row-simplify-col (nth matrix col) col)
        rest (drop (inc col) matrix)
        reduced-rest (map #(row-reduce reduced % col) rest)]
    (->> (concat [before reduced] reduced-rest)
         (filter (complement empty?)))))

(defn matrix-reduce-fully
  [matrix]
  (reduce (fn [coll item]
            (matrix-reduce-col coll item))
          (matrix-reduce-col matrix 0)
          (range 1 (count matrix)))
  )

(defn matrix-simplify-col
  [matrix col]
  (let [reducer (nth matrix col)
        before (take col matrix)
        after (drop (inc col) matrix)
        simplified-before (map #(row-reduce reducer % col) before)
        ]
    (->> (concat [(flatten simplified-before) reducer] after)
         (filter (complement empty?)))))

(defn matrix-simplify-fully
  [matrix]
  (let [length (count matrix)]
    (reduce (fn [coll item]
              (matrix-simplify-col coll item))
            (matrix-simplify-col matrix (dec length))
            (reverse (range 1 (dec length))))
    ))

(defn matrix-to-echelon
  [matrix]
  ;(println "Reducing form to echelon")
  ;(pp/pprint matrix)
  (->> (matrix-reduce-fully matrix)
       (matrix-simplify-fully)))

(defn determinant
  [matrix]
  (let [row1 (first matrix)
        row2 (second matrix)
        a (first row1)
        b (second row1)
        c (first row2)
        d (second row2)
        ]
    (- (* a d) (* b c))))

(defn solve-by-determinant
  [matrix extra]
  (let [row1 (first matrix)
        row2 (second matrix)
        vector-col [(+ extra (last row1)) (+ extra (last row2))]
        d (determinant matrix)
        dx (determinant [(assoc row1 0 (first vector-col))
                         (assoc row2 0 (second vector-col))])
        dy (determinant [(assoc row1 1 (first vector-col))
                         (assoc row2 1 (second vector-col))])
        ]
    [(/ dx d) (/ dy d)]))

(defn longest-running-consecutive
  ([numbers] (cond
               (= 0 (count numbers)) 0
               (= 1 (count numbers)) 1
               :else (longest-running-consecutive (rest numbers) (first numbers) 1 1))
   )
  ([numbers previous cur-max-running max-running]
   (loop [numbers-temp numbers
          previous-temp previous
          cur-max-running-temp cur-max-running
          max-running-temp max-running]
     (if (empty? numbers-temp)
       max-running-temp
       (let [head (first numbers-temp)
             is-consecutive? (or (= (inc previous-temp) head) (nil? previous-temp))
             new-max-running (if (true? is-consecutive?)
                               (inc cur-max-running-temp)
                               1)
             ]
         (recur (rest numbers-temp) (first numbers-temp) new-max-running (max new-max-running max-running-temp)))
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END of linear algebra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn quick-sort
  "docstring"
  [xs]
  (if (< (count xs) 1)
    xs
    (let [[pivot & rest] xs
          left (filter #(<= % pivot) rest)
          right (filter #(> % pivot) rest)]
      (concat (quick-sort left) [pivot] (quick-sort right)))))

(defn is-prime? [num]
  "Check if num is a prime number."
  (cond
    (< num 2) false
    (= num 2) true
    :else (not-any? zero? (map #(mod num %) (range 2 (inc (int (Math/sqrt num))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START of POLYGON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn to-vec
  "convert 2 points to vector"
  [point-a point-b]
  (let [new-x (- (first point-b) (first point-a))
        new-y (- (second point-b) (second point-a))
        ]
    {:x new-x :y new-y})
  )

(defn cross
  "docstring"
  [vec-a vec-b]
  (->> (- (* (:x vec-a) (:y vec-b))
          (* (:y vec-a) (:x vec-b)))
       (double)
       )
  )

(defn is-ccw?
  "note: to accept collinear points, we have to change the `> 0
   // returns true if point r is on the left side of line pq"
  [point-p point-q point-r]
  (> (cross (to-vec point-p point-q) (to-vec point-q point-r)) 0)
  )

(defn dot
  [vec-a vec-b]
  (->> (+ (* (:x vec-a) (:x vec-b))
          (* (:y vec-a) (:y vec-b)))
       (double)
       )
  )

(defn dist
  [point1 point2]
  (let [p1x-p2x (- (first point1) (first point2))
        p1y-p2y (- (second point1) (second point2))
        ]
    (clojure.math/hypot p1x-p2x p1y-p2y)))

(defn norm-sq
  "docstring"
  [vec-v]
  (->> (+ (* (:x vec-v) (:x vec-v))
          (* (:y vec-v) (:y vec-v))
          )
       (double)
       )
  )

(defn angle
  "Returns angle aob in rad"
  [point-a point-o point-b]
  (let [oa (to-vec point-o point-a)
        ob (to-vec point-o point-b)
        dot (dot oa ob)
        sqrt (clojure.math/sqrt (* (norm-sq oa) (norm-sq ob)))
        ]
    (clojure.math/acos (/ dot sqrt)))
  )

(defn is-on-polygon-edge?
  [point point-of-polygon]
  (cond
    (empty? point-of-polygon) false
    :else
    (loop [[x & xs] point-of-polygon
           on-polygon false
           ]
      (cond
        (empty? xs) on-polygon
        (true? on-polygon) true
        :else
        (let [dist-pi-p (dist x point)
              dist-p-pi1 (dist point (first xs))
              dist-pi+pi1 (dist x (first xs))
              is-on-polygon? (< (Math/abs (- (+ dist-pi-p dist-p-pi1) dist-pi+pi1)) EPS)
              ]
          (recur xs is-on-polygon?))))))

(defn in-polygon?
  "docstring"
  [point points-of-polygon with-polygon-edge]
  (cond
    (<= (count points-of-polygon) 3) false
    (true? (is-on-polygon-edge? point points-of-polygon)) with-polygon-edge
    :else
    (loop [[x & xs] points-of-polygon
           sum 0.0
           ]
      (cond
        (empty? xs) (< (Math/abs (- (Math/abs sum) (* 2 Math/PI))) EPS)
        (true? (is-ccw? point x (first xs))) (recur xs (+ sum (angle x point (first xs))))
        :else (recur xs (- sum (angle x point (first xs))))
        ))))
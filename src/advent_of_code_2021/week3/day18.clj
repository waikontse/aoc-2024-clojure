(ns advent-of-code-2021.week3.day18
  (:require [clojure.math.combinatorics :as combo]
            )
  )

(def example (slurp "./resources/y2021/week3/day18/example.txt"))
(def example2 (slurp "./resources/y2021/week3/day18/example2.txt"))
(def input (slurp "./resources/y2021/week3/day18/input.txt"))
(def simple-pair-regex #"\[(\d+),(\d+)]")
(def simple-pair-pattern (re-pattern "\\[\\d+,\\d+]"))
(def digits-matcher (re-pattern "\\d+"))

(defn add-number
  [left right]
  (str "[" left "," right "]"))

(defn can-explode-number?
  [number]
  (let [length (count number)]
    (loop [currPos 0
           currLevel 0]
      (cond
        (>= currLevel 5) true
        (= currPos length) false
        (= \[ (get number currPos)) (recur (inc currPos) (inc currLevel))
        (= \] (get number currPos)) (recur (inc currPos) (dec currLevel))
        (< currLevel 0) (throw (RuntimeException. "currLevel is negative"))
        :else (recur (inc currPos) currLevel)))))

(defn find-explotable-number
  "Returns the start and ending position with match E.g. {:match <string> :start <int> :end <int> }"
  [number]
  (let [length (count number)]
    (loop [currPos 0
           currLevel 0
           currStart 0
           currEnd 0
           matched nil]
      (cond
        ;; Stop conditions
        (< currLevel 0) (throw (RuntimeException. "currLevel is negative"))
        (>= currPos length) (throw (RuntimeException. (clojure.string/join ", " ["could not find explotable number" currPos currLevel currStart currEnd matched])))
        (and (= currLevel 4) (> currEnd 0)) {:match matched :start currStart :end currEnd}

        ;; intermediate conditions
        (and (< currLevel 5) (= \[ (get number currPos))) (recur (inc currPos) (inc currLevel) 0 0 nil)
        (and (< currLevel 5) (= \] (get number currPos))) (recur (inc currPos) (dec currLevel) 0 0 nil)
        (and (< currLevel 5) (= 0 currEnd)) (recur (inc currPos) currLevel 0 0 nil)

        ;; Start the matching and setting the start / end values
        (and (= currLevel 5) (= \[ (get number (dec currPos)))) (recur (inc currPos) 5 (dec currPos) 0 (str \[ (get number currPos)))
        (and (= currLevel 5) (not= \] (get number currPos))) (recur (inc currPos) 5 currStart 0 (str matched (get number currPos)))
        (and (= currLevel 5) (= \] (get number currPos))) (recur (inc currPos) 4 currStart currPos (str matched \]))

        :else (throw (RuntimeException. (clojure.string/join ", " ["unexpected error while finding explotable number" currPos currLevel currStart currEnd matched]))))))
  )


(defn find-all-numbers
  [number]
  (let [matcher (re-matcher digits-matcher number)]
    (loop [results []]
      (if (.find matcher)
        (recur (conj results {:match (Integer/parseInt (str (.group matcher)))
                              :start (.start matcher)
                              :end   (.end matcher)}))
        results))))

(defn can-split-number?
  [number]
  (let [digit-matches (find-all-numbers number)]
    (->> (map #(:match %) digit-matches)
         (some #(>= % 10)))))

(defn replace-string-index
  [s replace start-index stop-index]
  (let [left-side (subs s 0 start-index)
        right-side (subs s stop-index)
        new-string (str left-side replace right-side)
        ]
    new-string))

(defn split-number
  "docstring"
  [number-to-split]
  (let [leftmost-number-to-split (->> (find-all-numbers number-to-split)
                                      (filter #(>= (:match %) 10))
                                      first)
        leftmost-number (:match leftmost-number-to-split)
        splitted-number (str "[" (quot leftmost-number 2) "," (^[int int] Math/ceilDiv leftmost-number 2) "]")
        start-index (:start leftmost-number-to-split)
        end-index (:end leftmost-number-to-split)
        new-number (replace-string-index number-to-split splitted-number start-index end-index)
        ]
    new-number))

(defn explode-number
  "docstring"
  [number]
  (let [explotable-number (find-explotable-number number)
        [_, explotable-number-left, _] (re-matches simple-pair-regex (:match explotable-number))
        [_, _, explotable-number-right] (re-matches simple-pair-regex (:match explotable-number))
        number-left (last (find-all-numbers (subs number 0 (:start explotable-number))))
        number-right (first (find-all-numbers (subs number (:end explotable-number))))
        new-number-left (if (nil? number-left) nil (+ (:match number-left) (Integer/parseInt explotable-number-left)))
        new-number-right (if (nil? number-right) nil (+ (:match number-right) (Integer/parseInt explotable-number-right)))
        ;; Replace number on the right
        number-replaced-on-right (if (nil? new-number-right)
                                   number
                                   (replace-string-index number new-number-right
                                                         (+ (:end explotable-number) (:start number-right))
                                                         (+ (:end explotable-number) (:end number-right))))
        ;; Replace number on the left
        number-replaced-on-left (if (nil? new-number-left)
                                  number-replaced-on-right
                                  (replace-string-index number-replaced-on-right new-number-left
                                                        (:start number-left)
                                                        (:end number-left)))

        ;; Find the explotable pair again and replace with 0
        explotable-number (find-explotable-number number-replaced-on-left)
        pair-replaced-with-0 (replace-string-index number-replaced-on-left "0" (:start explotable-number) (inc (:end explotable-number)))
        ]
    pair-replaced-with-0))

(defn reduce-number
  "docstring"
  [number]
  ;; Explode number until no more explodes are possible, then split number until no more splits are possible,
  ;; then repeat until no more explodes or splits are possible.
  (loop [number-to-reduce number]
    (cond
      (true? (can-explode-number? number-to-reduce)) (recur (explode-number number-to-reduce))
      (true? (can-split-number? number-to-reduce)) (recur (split-number number-to-reduce))
      :else number-to-reduce
      )))

(defn find-simple-pair
  "Find the location of the first pair [\\d+,\\d+]. Return the pair and its location in the number."
  [number]
  (let [first-simple-pair (re-find simple-pair-pattern number)]
    first-simple-pair))

(defn magnitude-calc
  [simple-pair]
  (cond
    (nil? simple-pair) nil
    :else (let [[_, left, right] (re-matches simple-pair-regex simple-pair)]
            (+ (* 3 (Integer/parseInt left)) (* 2 (Integer/parseInt right))))))

(defn magnitude
  "docstring"
  [number]
  (loop [current-number number]
    (let [first-simple-pair (find-simple-pair current-number)
          pair-value (magnitude-calc first-simple-pair)]
      (cond
        (nil? first-simple-pair) (Integer/parseInt current-number)
        :else (recur (clojure.string/replace-first current-number first-simple-pair pair-value))
        ))))

(defn add-and-reduce-number
  [left right]
  (let [reduced-left (reduce-number left)
        reduced-right (reduce-number right)
        added-number (add-number reduced-left reduced-right)]
    added-number))

(defn add-and-reduce-all-numbers
  [numbers]
  (-> (reduce (fn [coll item]
                (add-and-reduce-number coll item))
              (first numbers)
              (rest numbers))
      (reduce-number)))

(defn solve-part-1
  [splitted-lines]
  (let [reduced-input (add-and-reduce-all-numbers splitted-lines)
        magnitude (magnitude reduced-input)]
    magnitude))

(defn solve-part-2
  [input]
  (let [splitted-lines (set (clojure.string/split-lines input))
        all-combos (combo/combinations splitted-lines 2)
        max-value (->> (pmap #(solve-part-1 %) all-combos)
                       (apply max))
        ]
    max-value))
(ns advent-of-code-2025.week2.day10
  (:require [clojure.pprint :as pp]
            [advent-of-code-2024.utils.io :as io]
            )
  )

(def example (slurp "./resources/y2025/day10/example.txt"))
(def input (slurp "./resources/y2025/day10/input.txt"))

(defn stringize-target
  [target left right]
  (-> (clojure.string/replace-first target left "")
       (clojure.string/replace-first right "")))

(defn get-button
  [raw-button]
  (-> (stringize-target raw-button "(" ")")
      (clojure.string/split #",")
      (io/strs->ints)))

(defn get-buttons
  [raw-buttons]
  (map #(get-button %) raw-buttons))

(defn parse-raw-line-to-puzzle
  "Parses a string to the puzzle input
  {
    :target [...]
    :current [...]
    :buttons (...)
    :presses [...]
    :config ...
  }
  "
  [raw-line]
  (let [raw-parts (clojure.string/split raw-line #" ")
        target (stringize-target (first raw-parts) "[" "]" )
        config (last raw-parts)
        buttons (->> (rest raw-parts)
                     drop-last
                     (get-buttons))
        current (apply str (repeat (count target) \.))
        presses (vec (repeat (count buttons) 0))
        ]
    {:target target
     :current current
     :buttons buttons
     :presses presses
     :config config
     }))

(defn parse-raw-lines-to-puzzle
  [raw-lines]
  (map #(parse-raw-line-to-puzzle %) raw-lines))

(defn get-new-state
  [current toggle-position]
  (let [curr-val (nth current toggle-position)
        new-val (str (if (= \. curr-val) \# \.))
        new-state (apply str (concat (subs current 0 toggle-position)
                                     new-val
                                     (subs current (inc toggle-position))))
        ]
    new-state))

(defn press-button
  [current button]
  (reduce (fn [current-state pos]
            (get-new-state current-state pos))
          current
          button)
  )

(defn perform-button-press
  "Returns a new puzzle state, with effect of button pressed"
  [puzzle-state button button-idx]
  (let [new-state (press-button (:current puzzle-state) button)
        old-presses (:presses puzzle-state)
        new-presses (assoc old-presses button-idx (inc (nth old-presses button-idx)))
        ]
    (assoc puzzle-state :presses new-presses :current new-state)))

(defn solve-puzzle
  [puzzle]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY puzzle)
         seen #{}
         ]
    (cond
      (empty? queue) (throw (Exception. "Queue is empty without solution."))
      (= (:current (peek queue)) (:target (peek queue))) (reduce + (:presses (peek queue)))
      (contains? seen (:current (peek queue))) (recur (pop queue) seen)
      :else
      (let [curr-puzzle-state (peek queue)
            new-states (map-indexed (fn [button-idx button]
                                    (perform-button-press curr-puzzle-state button button-idx))
                                  (:buttons puzzle))
            new-queue (pop queue)
            ]
        (recur (into new-queue new-states) (conj seen (:current (peek queue))))
        ))))

(defn solve-part-one
  [input]
  (let [raw-lines (clojure.string/split-lines input)
        puzzle-inputs (parse-raw-lines-to-puzzle raw-lines)
        puzzle-answers (->> puzzle-inputs
                            (pmap #(future (solve-puzzle %)))
                            (pmap deref))
        ]
    (reduce + puzzle-answers))
  )
;(time (solve-part-one input))
;; 466 - part1

;######################################################
; PART 2
;######################################################
(defn int-to-char
  "0 -> a, 1 -> b, etc."
  [num]
  (char (+ num (int \a))))

; 1. Generate standard constraints for all variables
; 2. Generate constraints for each position

(defn generate-basic-constraints
  "docstring"
  [puzzle-input]
  )

(defn generate-position-constraints
  [puzzle-input]
  )


;(def demo-puzzel {:target ".##.",
;                  :current "....",
;                  :buttons '((3) (1 3) (2) (2 3) (0 2) (0 1)),
;                  :presses [0 0 0 0 0 0],
;                  :config "{3,5,4,7}"})
;
;(solve-puzzle demo-puzzel)

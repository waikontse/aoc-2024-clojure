(ns advent-of-code-2025.week2.day10
  (:require [utils.io :as io]
            )
  (:import (org.ojalgo.optimisation Expression ExpressionsBasedModel Variable)))

(def example (slurp "./resources/y2025/day10/example.txt"))
(def example2 (slurp "./resources/y2025/day10/example2.txt"))
(def input (slurp "./resources/y2025/day10/input.txt"))

(defn stringize-target
  [target left right]
  (-> (clojure.string/replace-first target left "")
      (clojure.string/replace-first right "")))

(defn parse-ints
  [raw-button left right]
  (-> (stringize-target raw-button left right)
      (clojure.string/split #",")
      (io/strs->ints)))

(defn get-buttons
  [raw-buttons]
  (map #(parse-ints % "(" ")") raw-buttons))

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
        target (stringize-target (first raw-parts) "[" "]")
        config (parse-ints (last raw-parts) "{" "}")
        buttons (->> (rest raw-parts)
                     drop-last
                     (get-buttons))
        current (apply str (repeat (count target) \.))
        presses (vec (repeat (count buttons) 0))
        ]
    {:target  target
     :current current
     :buttons buttons
     :presses presses
     :config  config
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
(time (solve-part-one input))


;######################################################
; PART 2
;######################################################
(defn int-to-char
  "0 -> a, 1 -> b, etc."
  [num]
  (str (char (+ num (int \a)))))

; 1. Generate standard constraints for all variables
; 2. Generate constraints for each position
(defn create-integer-variable
  [model name]
  (doto (.addVariable model name)
    (.weight 1)
    (.lower 0)
    (.integer true)))

(defn generate-variables
  [puzzle-input model]
  (->> (:buttons puzzle-input)
       (map-indexed (fn [idx _]
                      (create-integer-variable model (int-to-char idx))))
       (zipmap (range))))

(defn generate-constraint-for-index
  [puzzle-input index target model variables]
  (let [partial-constraint-idxs (->> (map-indexed (fn [idx item]
                                                    (if (io/in? index item)
                                                      idx
                                                      nil))
                                                  (:buttons puzzle-input))
                                     (filter #(not (nil? %))))
        ^Expression expression (doto
                                 (.addExpression model)
                                 (.level ^long target))
        _ (doseq [var-index partial-constraint-idxs]
            (.set expression ^Variable (get variables var-index) 1))
        ]
    ))

(defn generate-position-constraints
  [puzzle-input model variables]
  (doseq [[idx item] (zipmap (range) (:config puzzle-input))]
    (generate-constraint-for-index puzzle-input idx item model variables)))

(defn solve-puzzle-part-2
  [puzzle-input]
  (let [model (ExpressionsBasedModel.)
        variables (generate-variables puzzle-input model)
        _ (generate-position-constraints puzzle-input model variables)
        result (.minimise model)
        ]
    (long (Math/ceil (.getValue result)))))

(defn solve-part-two
  [input]
  (let [raw-lines (clojure.string/split-lines input)
        puzzle-inputs (parse-raw-lines-to-puzzle raw-lines)
        answers (map #(solve-puzzle-part-2 %) puzzle-inputs)
        ]
    (reduce + answers)))

(time (solve-part-two input))
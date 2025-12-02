(ns advent-of-code-2025.week1.day1
  (:require [advent-of-code-2024.utils.io :as io]))

(def example (slurp "./resources/y2025/day1/example.txt"))
(def input (slurp "./resources/y2025/day1/input.txt"))

(defn split-command
  [move]
  (let [command (get move 0)
        steps (-> (subs move 1)
                  (io/str->int))]
    [command steps]))

(defn calc-pos-util
  [start-pos command]
  (let [split-command (split-command command)
        new-val (if (clojure.string/starts-with? command "L")
                  (- start-pos (get split-command 1))
                  (+ start-pos (get split-command 1)))
        ]
    new-val))

(defn calc-new-pos
  [start-pos command]
  (-> (calc-pos-util start-pos command)
      (mod 100)))

(defn calc-times-past-0
  [start-pos command]
  (let [new-val (calc-pos-util start-pos command)
        ended-on-zero? (zero? new-val)
        started-on-zero? (zero? start-pos)
        ]
    (cond
      (and (neg? new-val) started-on-zero?) (quot new-val -100)
      (and (neg? new-val) (not started-on-zero?)) (+ 1 (quot new-val -100))
      (true? ended-on-zero?) 1
      :else (quot new-val 100)
      )
    ))


(defn solve-part-1
  "docstring"
  [input]
  (loop [cur-pos 50
         commands (clojure.string/split-lines input)
         acc 0]
    (if (empty? commands)
      acc
      (let [new-pos (calc-new-pos cur-pos (first commands))
            adder (if (= 0 new-pos) 1 0)
            ]
        (recur new-pos (drop 1 commands) (+ acc adder))))))


(defn solve-part-2
  [input]
  (loop [cur-pos 50
         commands (clojure.string/split-lines input)
         acc 0
         ]
    (if (empty? commands)
      acc
      (let [new-pos (calc-new-pos cur-pos (first commands))
            adder (calc-times-past-0 cur-pos (first commands))
            ]
        (recur new-pos (drop 1 commands) (+ acc adder))
        ))))





;; 1129
;;(println (solve-part-1 input))
;; 6638
;;(println (solve-part-2 input))

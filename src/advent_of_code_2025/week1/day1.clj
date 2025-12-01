(ns advent-of-code-2025.week1.day1
  (:require [advent-of-code-2024.utils.io :as io]))

(def example (slurp "./resources/y2025/day1/example.txt"))
(def input (slurp "./resources/y2025/day1/input.txt"))

(defn solve-part-1
  "docstring"
  [input]
  (loop [acc [50]
         commands (clojure.string/split-lines input)]
    (if (empty? commands)
      (->> (filter #(= 0 %) acc)
           (count))
      (let [start-pos (last acc)
            new-pos (calc-new-pos start-pos (first commands))
            ;_ (println "in call: " start-pos new-pos)
            ]
        (recur (conj acc new-pos) (drop 1 commands))))))

(defn solve-part-2
  [input]
  (loop [acc [50]
         commands (clojure.string/split-lines input)
         extra-pointers-to-0 0
         ]
    (if (empty? commands)
      (->> (filter #(= 0 %) acc)
           (count)
           (+ extra-pointers-to-0))
      (let [start-pos (last acc)
            new-pos (calc-new-pos start-pos (first commands))
            extra-pointers-to-0-tmp (calc-times-past-0 start-pos (first commands))
            ]
        (recur (conj acc new-pos)
               (drop 1 commands)
               (+ extra-pointers-to-0 extra-pointers-to-0-tmp))
        ))))

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
  0)


(defn split-command
  [move]
  (let [command (get move 0)
        steps (-> (subs move 1)
                  (io/str->int))]
    [command steps]))



(println (solve-part-1 input))
(println (solve-part-2 example))

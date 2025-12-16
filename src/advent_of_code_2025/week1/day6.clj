(ns advent-of-code-2025.week1.day6
  (:require [advent-of-code-2024.utils.io :as io]
            ))

(def example (slurp "./resources/y2025/day6/example.txt"))
(def input (slurp "./resources/y2025/day6/input.txt"))

(defn raw-string-to-ints
  [raw-string]
  (->> (clojure.string/split (clojure.string/trim raw-string) #" +")
       (map #(io/str->int %)))
  )

(raw-string-to-ints "6 7 8 9")

(defn parse-to-raw-lines
  [raw-input]
  (let [raw-split-lines (clojure.string/split-lines raw-input)
        raw-split-lines-without-last (drop-last raw-split-lines)
        ;;_ (println "raw split without last: " raw-split-lines-without-last)
        last-line (clojure.string/split (last raw-split-lines) #" +")
        integer-split-lines (map #(raw-string-to-ints %) raw-split-lines-without-last)
        combined (conj integer-split-lines last-line)
        ;_ (println  "combined: " combined)
        ]
    combined)
  )

(println (apply map vector (parse-to-raw-lines example)))

(defn eval-line
  "docstring"
  [line]
  (let [op (first line)
        args (rest line)
        ]
    (cond
      (= "+" op) (apply + args)
      (= "*" op) (apply * args)
      ))
  )

(defn solve-part-1
  "docstring"
  [input]
  (let [parsed-input (apply map vector (parse-to-raw-lines input))
        ;_ (println parsed-input)
        eval-parsed-input (map #(eval-line %) parsed-input)
        ]
    (apply + eval-parsed-input))
  )
(solve-part-1 input)


(defn solve-part-2
  "docstring"
  [input]
  0)

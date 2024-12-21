(ns advent-of-code-2024.week2.day11
  (:require [advent-of-code-2024.utils.io :as io]))

(defn split-stone
  [stone]
  (let [stone-str (str stone)
        length (count stone-str)
        mid-point (/ length 2)]
    [(io/str->int (subs stone-str 0 mid-point)) (io/str->int (subs stone-str mid-point))]))

(defn move-stone
  [stone]
  (cond
    (= 0 stone) 1
    (even? (count (str stone))) (split-stone stone)
    :else (* stone 2024)))

(defn move-stones
  "docstring"
  [stones rounds]
  )

(move-stone 99)

(defn solve-part-1
  "docstring"
  [arglist]
  0)

(defn solve-part-2
  "docstring"
  [arglist]
  0)
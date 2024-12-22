(ns advent-of-code-2024.week2.day11
  (:require [advent-of-code-2024.utils.io :as io]))

(defn split-stone
  [stone acc]
;  (println "splitting stone" stone)
  (let [stone-str (str stone)
        length (count stone-str)
        mid-point (/ length 2)
        left-side (io/str->int (subs stone-str 0 mid-point))
        right-side (io/str->int (subs stone-str mid-point))]
    (-> acc
        (conj left-side)
        (conj right-side))))

(defn move-stone
  [stone acc]
 ; (println "move stone:" stone)
  (cond
    (= 0 stone) (conj acc 1)
    (even? (count (str stone))) (split-stone stone acc)
    :else (conj acc (* stone 2024))))

(defn move-stones
  [stones rounds]
 ; (println "moving stones:" stones)
  (loop [stones stones
         rounds rounds]
    (if (= 0 rounds)
      stones
      (let [updated-stones (reduce #(move-stone %2 %1) [] stones)
            ;_ (println "updated stones" updated-stones)
            ]
        (recur updated-stones (dec rounds))))))



(defn solve-part-1
  [filename]
  (let [raw-lines (io/read-input "day11/input.txt")
        _ (println "raw line:" raw-lines)
        splitted-lines (clojure.string/split (first raw-lines) #" ")
        _ (println "splitted" splitted-lines)
        parsed-lines (->> splitted-lines
                          (map #(io/str->int %)))
        _ (println "parsed line:" parsed-lines)
        pebbles (move-stones parsed-lines 1)
        ]
    (count pebbles))
  )

(defn solve-part-2
  "docstring"
  [arglist]
  0)
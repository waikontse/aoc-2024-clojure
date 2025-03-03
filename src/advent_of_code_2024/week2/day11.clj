(ns advent-of-code-2024.week2.day11
  (:require [advent-of-code-2024.utils.io :as io]
            [clojure.pprint :as pp]))

(defn split-stone
  [stone acc]
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
  (cond
    (= 0 stone) (conj acc 1)
    (even? (count (str stone))) (split-stone stone acc)
    :else (conj acc (* stone 2024))))

(defn move-stones
  [stones rounds]
  (loop [stones stones
         rounds rounds]
    (if (= 0 rounds)
      stones
      (let [updated-stones (reduce #(move-stone %2 %1) [] stones)]
        (recur updated-stones (dec rounds))))))

(def memoized-move-stones (memoize move-stones))

(defn solve-part-1
  [filename]
  (let [raw-lines (io/read-input "day11/example.txt")
        _ (println "raw line:" raw-lines)
        splitted-lines (clojure.string/split (first raw-lines) #" ")
        _ (println "splitted" splitted-lines)
        parsed-lines (->> splitted-lines
                          (map #(io/str->int %)))
        _ (println "parsed line:" parsed-lines)
        pebbles (memoized-move-stones parsed-lines 25)
        ]
    (count pebbles)))

(defn multiply-values-for-key
  [frequency-table multiplier]
  (reduce (fn [coll item]
            (let [[k v] item]
              (into coll [[k (* multiplier v)]])))
          {}
          frequency-table))

(defn run-frequencies-25-times
  [frequencies-table]
  (let [expanded-frequencies-table (pmap (fn [[k v]]
                                          (-> (frequencies (memoized-move-stones [k] 25))
                                              (multiply-values-for-key v)))
                                        frequencies-table)
        ]
    (apply merge-with + expanded-frequencies-table)))

(defn solve-for-75
  "Since have already calculated a number for 25 rounds, we can combime it.
   1. Get the round of 25 for a number. Get the frequencies of that result 1.
   2. For each of those keys, we
      a. calculate it till round 25. Get the frequencies and multiply those values with the value from 1
      b. We need to combine the frequencies for all the keys
   3. We do it 1 last round of 25 for all the keys in the frequency set from step 2 * frequencies"
  [number]
  )

(defn solve-part-2
  [filename]
  (let [raw-lines (io/read-input "day11/input.txt")
        _ (println "raw line:" raw-lines)
        splitted-lines (clojure.string/split (first raw-lines) #" ")
        _ (println "splitted" splitted-lines)
        parsed-lines (->> splitted-lines
                          (map #(io/str->int %)))
        pebbles (memoized-move-stones parsed-lines 25)
        set-pebbles (frequencies pebbles)
        fifty-times (run-frequencies-25-times set-pebbles)
        seventy-five-times (run-frequencies-25-times fifty-times)
        _ (println "unique vals: " (count seventy-five-times))
        sum-of-values (reduce + (vals seventy-five-times))
        _ (println "sum of values: " sum-of-values)
        ]
    sum-of-values)
  )

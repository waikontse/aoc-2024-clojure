(ns advent-of-code-2025.week1.day2
  (:require [advent-of-code-2024.utils.io :as io])
  (:require [clojure.pprint :as pprint])
  )

(def example (clojure.string/trim (slurp "./resources/y2025/day2/example.txt")))
(def input (slurp "./resources/y2025/day2/input.txt"))

(defn split-range-into-spec
  [range]
  (let [splitted (clojure.string/split (clojure.string/trim range) #"-")
        lower (io/str->int (get splitted 0))
        upper (io/str->int (get splitted 1))
        ]
    {"lower" lower "upper" upper}))

(defn is-mirrored?
  [target partition-count]
  (let [str-format (str target)
        length (count str-format)
        n (int (Math/ceil (/ length partition-count)))
        partitions (partition n n "z" str-format)
        ]
    (and (not= 1 length) (apply = partitions))))

(defn gen-candidates
  [spec]
  (range (get spec "lower") (inc (get spec "upper"))))

(defn find-mirrors-for-spec
  [spec max-partitions]
  (let [values (gen-candidates spec)
        partition-sizes (range 2 (inc max-partitions))
        filtered-values (map (fn [partition-size]
                               (filter #(is-mirrored? % partition-size) values))
                             partition-sizes)]
    (->> (flatten filtered-values)
         (distinct))))

(defn max-length-spec
  [spec]
  (->> (get spec "upper")
       (str)
       (count)))

(defn solve-problem
  [input calc]
  (let [splitted (clojure.string/split input #",")
        specs (map #(split-range-into-spec %) splitted)
        valid-values-for-specs (calc specs)
        ]
    (->> (flatten valid-values-for-specs)
         (apply +)
         )))


(defn solve-part-1
  [input]
  (solve-problem input
                 (fn [specs] (map #(find-mirrors-for-spec % 2) specs))))

(defn solve-part-2
  [input]
  (solve-problem input
                 (fn [specs] (map (fn [spec] (find-mirrors-for-spec spec (max-length-spec spec))) specs))))
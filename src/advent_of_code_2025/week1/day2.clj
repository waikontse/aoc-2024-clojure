(ns advent-of-code-2025.week1.day2
  (:require [advent-of-code-2024.utils.io :as io])
  (:require [clojure.pprint :as pprint])
  )

(def example (clojure.string/trim (slurp "./resources/y2025/day2/example.txt")))
(def input (slurp "./resources/y2025/day2/input.txt"))

(defn split-range-into-spec
  [range]
  (let [splitted (clojure.string/split (clojure.string/trim range) #"-")
        ;;_ (println "range and splitted" range splitted)
        lower (io/str->int (get splitted 0))
        upper (io/str->int (get splitted 1))
        ;;_ (println "lower and upper" lower upper)
        ]
    {"lower" lower "upper" upper})
  )

(defn is-mirrored-faulty?
  [target partition-count]
  (let [str-format (str target)
        length (count str-format)
        n (int (Math/ceil (/ length partition-count)))
        ;;_ (println "partition size:" n)
        partitions (partition n n "z" str-format)
        ;;_ (clojure.pprint/pprint partitions)
        ]
    (and (not= 1 length) (apply = partitions))))

(is-mirrored-faulty? 1212121212 2)

(defn gen-candidates
  [spec]
  (range (get spec "lower") (inc (get spec "upper"))))


(defn find-mirrors-for-spec
  [spec max-partitions]
  (let [values (gen-candidates spec)
        partition-sizes (range 2 (inc max-partitions))
        filtered-values (map (fn [partition-size]
                               (filter #(is-mirrored-faulty? % partition-size) values))
                             partition-sizes)]
    (->> (flatten filtered-values)
         (distinct))))


(defn solve-part-1
  [input]
  (let [splitted (clojure.string/split input #",")
        specs (map #(split-range-into-spec %) splitted)
        valid-values-for-specs (map #(find-mirrors-for-spec % 2) specs)
        ]
    (->> (flatten valid-values-for-specs)
         (apply +)
         )))


(defn max-length-spec
  [spec]
  (->> (get spec "upper")
       (str)
       (count))
  )

(max-length-spec {"lower" 11 "upper" 1141})

(defn solve-part-2
  [input]
  (let [splitted (clojure.string/split input #",")
        specs (map #(split-range-into-spec %) splitted)
        valid-values-for-specs (map (fn [spec]
                                      (find-mirrors-for-spec spec (max-length-spec spec)))
                                    specs)
        ]
    (->> (flatten valid-values-for-specs)
         (apply +)
         )))

;; 4174379265
(solve-part-2 input)

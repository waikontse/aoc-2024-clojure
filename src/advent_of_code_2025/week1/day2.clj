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
        _ (clojure.pprint/pprint partitions)
        ]
    (and (not= 1 length) (apply = partitions))))

(is-mirrored-faulty? 1212121212 2)

(defn is-mirrored-simple?
  [target]
  (let [str-format (str target)
        half-point (/ (count str-format) 2)
        left (subs str-format 0 half-point)
        right (subs str-format half-point)
        ;;_ (println "partition size:" n)
        ;;_ (clojure.pprint/pprint partitions)
        ]
    (= left right)))

(is-mirrored-simple? 99)

(defn gen-candidates
  [spec]
  (range (get spec "lower") (inc (get spec "upper"))))

(defn find-mirrors-for-spec
  [spec]
  (let [values (gen-candidates spec)
        filtered-values (filter #(is-mirrored-faulty? % 2) values)
        ]
    filtered-values))

(defn solve-part-1
  [input]
  (let [splitted (clojure.string/split input #",")
        specs (map #(split-range-into-spec %) splitted)
        valid-values-for-specs (map #(find-mirrors-for-spec %) specs)
        ;;_ (clojure.pprint/pprint specs)
        _ (clojure.pprint/pprint valid-values-for-specs)
        ]
    (->> (flatten valid-values-for-specs)
         (apply +)
         )))

;; 1227775554
(solve-part-1 example)

;; 21898734247
(println (solve-part-1 input))

(defn solve-part-2
  ""
  [input]
  )

;; 4174379265


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

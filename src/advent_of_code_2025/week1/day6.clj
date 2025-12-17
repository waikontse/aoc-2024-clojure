(ns advent-of-code-2025.week1.day6
  (:require [advent-of-code-2024.utils.io :as io]
            ))

(def example (slurp "./resources/y2025/day6/example.txt"))
(def input (slurp "./resources/y2025/day6/input.txt"))

(defn raw-string-to-ints
  [raw-string]
  (->> (clojure.string/split (clojure.string/trim raw-string) #" {1,1}")
       ;(map #(io/str->int %))
       )
  )
(raw-string-to-ints "123 328  51 64")

(clojure.string/split-lines example)

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

(parse-to-raw-lines example)

(println (apply map vector (parse-to-raw-lines example)))

(defn eval-line
  [line transformer]
  (let [op (first line)
        args (transformer (rest line))
        ]
    (cond
      (= "+" op) (apply + args)
      (= "*" op) (apply * args)
      ))
  )

;; 4277556
;; 5335495999141
(defn solve-part-1
  [input]
  (let [parsed-input (apply map vector (parse-to-raw-lines input))
        eval-parsed-input (map #(eval-line % identity) parsed-input)
        ]
    (apply + eval-parsed-input))
  )
(solve-part-1 input)


(defn transpose-numbers
  [coll]
  (let [_ (println "transposing: " coll)
        str-coll (map #(str %) coll)
        max-length (apply max (map #(count %) str-coll))
        transposed (->> (range max-length)
                        (map (fn [pos]
                               (map #(get % pos) str-coll)))
                        (map #(apply str %))
                        (map #(io/str->int %)))
        _ (clojure.pprint/pprint transposed)]
    transposed)
  )

;; 3263827

(defn solve-part-2
  [input]
  (let [parsed-input (apply map vector (parse-to-raw-lines input))
        eval-parsed-input (map #(eval-line % transpose-numbers) parsed-input)
        ]
    (apply + eval-parsed-input))
  )

;The rightmost problem is 4 + 431 + 623 = 1058
;The second problem from the right is 175 * 581 * 32 = 3253600
;The third problem from the right is 8 + 248 + 369 = 625
;Finally, the leftmost problem is 356 * 24 * 1 = 8544
;Now, the grand total is 1058 + 3253600 + 625 + 8544 = 3263827.

(solve-part-2 example)



(transpose-numbers ["64" "23" "314"])
(transpose-numbers [64 23 314])
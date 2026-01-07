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
        last-line (clojure.string/split (last raw-split-lines) #" +")
        integer-split-lines (map #(raw-string-to-ints %) raw-split-lines-without-last)
        combined (conj integer-split-lines last-line)
        ]
    combined)
  )

(println (apply map vector (parse-to-raw-lines example)))

(defn eval-line
  [line]
  (let [op (first line)
        args (rest line)
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
        eval-parsed-input (map #(eval-line %) parsed-input)
        ]
    (apply + eval-parsed-input))
  )
(solve-part-1 input)


;;;;;;; Solution part 2
(defn eval-line-2
  [lines]
  (let [op (first lines)
        rest (rest lines)
        converted-rest (->> (map #(clojure.string/trim %) rest)
                            (io/strs->ints))
        ]
    (cons op converted-rest)))

(defn get-nth-for-all
  [nth strings]
  (apply str (map #(get % nth) strings)))

(defn prepare-numbers
  [from to-exclusive lines]
  (map #(get-nth-for-all % lines) (range from to-exclusive))
  )

(defn find-split-indexes
  [lines]
  (let [numbers (drop-last lines)
        ops (clojure.string/split (last lines) #" +")
        splits (for [x (range 0 (count (first numbers)))
                     :let [column (get-nth-for-all x numbers)
                           split x]
                     :when (clojure.string/blank? column)
                     ]
                 split)
        ]
    {:split splits :ops ops}))

(defn insert-start-indexes-between
  [xs]
  (reduce (fn [coll val]
            (-> (conj coll val)
                (conj (inc val))))
          [0]
          xs))

(defn pad-with-end
  [xs end]
  (conj xs end))

(defn calculate-number
  [arg lines]
  (let [[coord op] arg
        [low high] coord
        ]
    (->> (prepare-numbers low high lines)
         (cons op)
         (eval-line-2)
         (eval-line))))

(defn solve-part-2
  [input]
  (let [splitted (clojure.string/split-lines input)
        splitted-without-ops (drop-last splitted)
        splits-and-ops (find-split-indexes splitted)
        max-line-length (apply max (map #(count %) splitted))
        new-splits-with-end (pad-with-end (insert-start-indexes-between (:split splits-and-ops)) max-line-length)
        partitions (partition 2 new-splits-with-end)
        partitions-with-ops (map vector partitions (:ops splits-and-ops))
        calculated-numbers (map #(calculate-number % splitted-without-ops) partitions-with-ops)
        sum (apply + calculated-numbers)
        ]
    sum)
  )

;; 10142723156431
(solve-part-2 input)

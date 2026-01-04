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

;; 4277556
;; 5335495999141
(defn solve-part-1
  [input]
  (let [parsed-input (apply map vector (parse-to-raw-lines input))
        ;_ (println parsed-input)
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
    (cons op converted-rest))
  )

(eval-line (eval-line-2 ["+" "123" " 45" "  6"]))

(defn get-nth-for-all
  [nth strings]
  (apply str (map #(get % nth) strings)))


(defn prepare-numbers
  [from to-inclusive lines]
  (map #(get-nth-for-all % lines) (range from (inc to-inclusive)))
  )

(defn find-split-indexes
  [lines]
  (let [numbers (drop-last lines)
        ops (clojure.string/split (last lines) #" +")
        ]
    (for [x (range 0 (count (first numbers)))
          :let [column (get-nth-for-all x numbers)
                split x]
          :when (clojure.string/blank? column)
          ]
      split)
    )
  )

(defn solve-part-2
  "docstring"
  [input]
  0)



(->> (clojure.string/split-lines example)
     (drop-last)
     (prepare-numbers 0 2)
     (cons "*")
     (eval-line-2)
     (eval-line))


;; experiments to solve part 2

(find-split-indexes (clojure.string/split-lines example))

(get-nth-for-all 3 (clojure.string/split-lines example))
(get-nth-for-all 0 ["1 2 3" "4 5 6" "7 8 9"])

(io/str->int (clojure.string/trim "   4"))

(clojure.string/blank? "   ")
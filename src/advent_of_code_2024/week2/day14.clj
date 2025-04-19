(ns advent-of-code-2024.week2.day14
  (:require [advent-of-code-2024.utils.io :as io]
            [clojure.pprint :as pp]
            [advent-of-code-2024.utils.algorithms :as algo]
            )
  )

(def pOrV #"(p\=|v\=)")

(defn clean-string
  [line]
  (clojure.string/replace line pOrV ""))

(defn splitted-to-pair
  "E.g. 9,4 -> {:x 9 :y 4}"
  [raw-splitted]
  (let [raw-numbers (clojure.string/split raw-splitted #",")
        left (io/str->int (first raw-numbers))
        right (io/str->int (second raw-numbers))]
    [left right]))


(defn parse-to-spec
  [raw-line]
  (let [clean-line (clean-string raw-line)
        splitted (clojure.string/split clean-line #" ")
        position (splitted-to-pair (first splitted))
        velocity (splitted-to-pair (second splitted))
        ]
    {:position position :velocity velocity})
  )

;; multiply the position by velocity
;; normalize to the grid size
;; partition to the quadrants

(defn multiply-position
  ""
  [robot multiplier]
  (let [new-values (algo/row-multiply-by multiplier)]
    )


  )

(def pos  {:position {:x 9, :y 3}, :velocity {:x 2, :y 3}})
((comp :x :position) pos)

(defn solve-part-1
"docstring"
[filename]
(let [lines (io/read-input "day14/example.txt")
                                        ;_ (println lines)
      cleaned-lines (map #(clean-string %) lines)
      _ (pp/pprint cleaned-lines)
      _ (pp/pprint (map #(parse-to-spec %) cleaned-lines))
      ])
)

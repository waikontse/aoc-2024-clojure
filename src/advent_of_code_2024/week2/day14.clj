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
  [{:keys [position velocity]}
   multiplier]
  (let [new-velocity (algo/row-multiply-by velocity multiplier)
        new-values (map vector position new-velocity)
        ]
    (map #(apply + %) new-values))
  )

(defn normalize-position
  "Normalizes a position."
  [position
   {:keys [width height]}
   ]
  [(mod (first position) width) (mod (second position) height)]
  )

(defn split-into-quadrants
  ""
  [position
   {:keys [width height]}]
  (let [ ]
    )


  "")



(defn solve-part-1
  [filename]
  (let [lines (io/read-input "day14/example.txt")
                                        ;_ (println lines)
        cleaned-lines (map #(clean-string %) lines)
        parsed-lines (map #(parse-to-spec %) cleaned-lines)
        _ (pp/pprint cleaned-lines)
        _ (pp/pprint parsed-lines)
        all-new-values (map #(multiply-position % 100) parsed-lines)
        _ (pp/pprint all-new-values)
        all-normalized (map #(normalize-position % {:width 11 :height 7}) all-new-values)
        _ (pp/pprint all-normalized)
        ])
  )

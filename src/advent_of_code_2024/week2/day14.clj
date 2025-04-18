(ns advent-of-code-2024.week2.day14
  (:require [advent-of-code-2024.utils.io :as io]
            [clojure.pprint :as pp])
  )

(def pOrV #"(p\=|v\=)")

(defn clean-string
  [line]
  (clojure.string/replace line pOrV ""))

(defn parse-to-spec
  [raw-line]
  (let [clean-line (clean-string raw-line)
        position "todo"
        velocity "todo"
        ]
    {:position position :velocity velocity})
  )

(defn solve-part-1
  "docstring"
  [filename]
  (let [lines (io/read-input "day14/example.txt")
        ;_ (println lines)
        cleaned-lines (map #(clean-string %) lines)
        _ (pp/pprint cleaned-lines)
        ])
  )

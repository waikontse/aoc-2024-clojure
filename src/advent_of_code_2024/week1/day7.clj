(ns advent-of-code-2024.week1.day7
  (:require [advent-of-code-2024.utils.io :as io])
  )

(defn parse-line
  "docstring"
  [raw-line]
  (let [splitted (clojure.string/split raw-line #": ")
        sum (io/str->int (first splitted))
        _ (println "second" (second splitted))
        operands (->> (second splitted)
                      (clojure.string/trim))
        splitted-operands (->> (clojure.string/split operands #" ")
                            (map #(io/str->int %)))
        ]
    {:sum sum :operands splitted-operands}))

(parse-line "190: 10 90")

(defn execute
  "docstring"
  [op operand1 operand2]
  (op operand1 operand2))

(defn run-combinations
  [[op1 op2 & xs] operators]
  (println operators)
  (loop [op1 op1
         op2 op2
         xs xs
         operators operators]
    (let [val ((first operators) op1 op2)]
      (if (empty? xs)
        val
        (recur val (first xs) (rest xs) (rest operators))))))

(run-combinations [10 90 27 1] [+ * +])


(defn solve-part-1
  "docstring"
  [filename]
  )

(defn solve-part-2
  "docstring"
  [filename]
  )

(execute + 1 2)



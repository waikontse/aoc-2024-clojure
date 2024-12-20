(ns advent-of-code-2024.week1.day7
  (:require [advent-of-code-2024.utils.io :as io])
  (:require [clojure.math.combinatorics :as combo])
  )

(defn parse-line
  [raw-line]
  (let [splitted (clojure.string/split raw-line #": ")
        sum (io/str->int (first splitted))
        operands (->> (second splitted)
                      (clojure.string/trim))
        splitted-operands (->> (clojure.string/split operands #" ")
                            (map #(io/str->int %)))]
    {:sum sum :operands splitted-operands}))

(defn execute
  [op operand1 operand2]
  (cond
    (= "*" op) (* operand1 operand2)
    (= "+" op) (+ operand1 operand2)
    (= "||" op) (io/str->int (str operand1 operand2))))

(defn run-combination
  [[op1 op2 & xs] operators]
  (loop [op1 op1
         op2 op2
         xs xs
         operators operators]
    (let [val (execute (first operators) op1 op2)]
      (if (empty? xs)
        val
        (recur val (first xs) (rest xs) (rest operators))))))

(defn solve-entry
  [puzzle allowed-ops]
  (let [operators (combo/selections allowed-ops (dec (count (:operands puzzle))))
        answers (-> (map #(run-combination (:operands puzzle) %) operators)
                    set)]
    (if (contains? answers (:sum puzzle))
      (:sum puzzle)
      0)))

(defn solve-part-1
  [filename]
  (let [raw-lines (io/read-input filename)
        parsed-lines (map parse-line raw-lines)
        all-answers (map #(solve-entry % ["+" "*"]) parsed-lines)]
    (reduce + all-answers)))

(defn solve-part-2
  [filename]
  (let [raw-lines (io/read-input filename)
        parsed-lines (map parse-line raw-lines)
        all-answers (map #(solve-entry % ["+" "*" "||"]) parsed-lines)]
    (reduce + all-answers)))
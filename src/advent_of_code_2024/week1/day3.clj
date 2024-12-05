(ns advent-of-code-2024.week1.day3
  (:require [advent-of-code-2024.utils.io :as io]))

(defn categorize-op
  [raw-op]
  (cond
    (clojure.string/starts-with? raw-op "mul(") :mul
    (clojure.string/starts-with? raw-op "do(") :do
    (clojure.string/starts-with? raw-op "don't(") :don't))


(defmulti parse-op categorize-op)

(defmethod parse-op :mul
  [raw-op]
  (let [raw-operands (-> raw-op
                         (clojure.string/replace #"mul\(" "")
                         (clojure.string/replace #"\)" "")
                         (clojure.string/split #","))
        ]
    {:op :mul :left (first raw-operands) :right (second raw-operands)}))

(defmethod parse-op :do
  [raw-op]
  {:op :do})

(defmethod parse-op :don't
  [raw-op]
  {:op :don't})

(defmethod parse-op :default
  [args]
  )


(defn parse-line
  [raw-line]
  "return a map of operands eg: {:left <int> :right <int> :op mul}"
  (let [operands (re-seq #"mul\(\d{1,3},\d{1,3}\)|don't\(\)|do\(\)" raw-line)
        _ (println operands)]
    (map parse-op operands)))

(defn execute-operand
  [operand]
  ;(println "executing op: " operand)
  (* (io/str->int (:left operand)) (io/str->int (:right operand))))

(defn solve-part-1
  "docstring"
  [filename]
  (let [raw-lines (io/read-input filename)
        parsed-operands (flatten (map parse-line raw-lines))
        results (map execute-operand parsed-operands)
        ]
    ;(run! println parsed-operands)
    ;(println results)
    (reduce + results)))


(defn solve-part-2
  "docstring"
  [filename]
  (let [raw-lines (io/read-input "day3/example.txt")
        parsed-operands (flatten (map parse-line raw-lines))
        ; results (map execute-operand parsed-operands)
        ]
    (println parsed-operands)
    )
  )

(parse-line "mul(123,123)")
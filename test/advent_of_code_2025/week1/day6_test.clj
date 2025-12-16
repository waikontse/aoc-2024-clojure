(ns advent-of-code-2025.week1.day6-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2025.week1.day6 :refer :all]))

(def example (slurp "./resources/y2025/day6/example.txt"))
(def input (slurp "./resources/y2025/day6/input.txt"))

(deftest solve-part-1-test
  (testing "Correct values"
    (is (= 4277556 (solve-part-1 example))))
  (testing "Correct values for normal input"
    (is (= 5335495999141 (solve-part-1 input))))
  )



(deftest solve-part-2-test
  (testing "Correct values for part 2"
    (is (= 3263827 (solve-part-2 example)))))
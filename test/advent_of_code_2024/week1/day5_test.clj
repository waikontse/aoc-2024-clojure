(ns advent-of-code-2024.week1.day5-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2024.week1.day5 :refer :all]))

(deftest solve-part-1-test
  (testing "Solve part 1 using example data"
    (is (= 143 (solve-part-1 "day5/example.txt"))))
  (testing "Solve part 1 using input data"
    (is (= 4996 (solve-part-1 "day5/input.txt"))))
  )
(ns advent-of-code-2024.week1.day1.solution-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2024.week1.day1.solution :refer :all]))

(deftest solve-part-1-test
  (testing "part1 with example")
  (is (= 11 (solve-part-1 "day1/example.txt"))))

(deftest solve-part-2-test
  (testing "part2 with example")
  (is (= 31 (solve-part-2 "day1/example.txt"))))

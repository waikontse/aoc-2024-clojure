(ns advent-of-code-2024.week1.day1-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2024.week1.day1 :refer :all]))

(deftest solve-part-1-test
  (testing "part1 with example")
  (is (= 11 (solve-part-1 "day1/example.txt"))))

(deftest solve-part-1-test
  (testing "part1 with real input")
  (is (= 1580061 (solve-part-1 "day1/input.txt"))))

(deftest solve-part-2-test
  (testing "part2 with example")
  (is (= 31 (solve-part-2 "day1/example.txt"))))

(deftest solve-part-2-test
  (testing "part2 with real input")
  (is (= 23046913 (solve-part-2 "day1/input.txt"))))

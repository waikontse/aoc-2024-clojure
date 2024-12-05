(ns advent-of-code-2024.week1.day3-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2024.week1.day3 :refer :all]))

(deftest solve-part-1-example-test
  (testing "solution for day 3 part 1")
  (is (= (solve-part-1 "day3/example.txt") 161)))

(deftest solve-part-1-input-test
  (testing "solution for day 3 part 1")
  (is (= (solve-part-1 "day3/input.txt") 173785482)))

(deftest solve-part-2-example-test
  (testing "solution for day 3 part 2")
  (is (= (solve-part-2 "day3/example.txt") 48)))


(deftest solve-part-2-test
  (testing "solution for day 3 part 2")
  (is (= (solve-part-2 "day3/input.txt") 83158140)))

(ns advent-of-code-2024.week1.day4-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2024.week1.day4 :refer :all]))

(deftest solve-part-1-test
  (testing "Solve part 1"
    (testing "with sample data"
      (is (= 18 (solve-part-1 "day4/example.txt"))))
    (testing "with real data"
      (is (= 2575 (solve-part-1 "day4/input.txt"))))))


(deftest solve-part-2-test
  (testing "Solve part 2"
    (testing "with sample data"
      (is (= 9 (solve-part-2 "day4/example.txt"))))
    (testing "with real data"
      (is (= (solve-part-2 "day4/input.txt") 2575)))))

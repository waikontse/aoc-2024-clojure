(ns advent-of-code-2024.week1.day2-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2024.week1.day2 :refer :all]))

(deftest solve-part-1-test
  (testing "part 1 with sample data")
  (is (= 2 (solve-part-1 "day2/example.txt"))))

(deftest solve-part-1-test
  (testing "part 1 with real input data")
  (is (= 299 (solve-part-1 "day2/input.txt"))))


(deftest solve-part-2-test
  (testing "part 2 with sample data")
  (is (= 4 (solve-part-2 "day2/example.txt"))))

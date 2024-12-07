(ns advent-of-code-2024.week1.day4-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2024.week1.day4 :refer [solve-part-1]]))

(deftest solve-part-1-test
  (testing "Solve part 1"
    (testing "with sample data"
      (is (= (solve-part-1 "day4/example.txt") 18)))
    (testing "with real data"
      (is (= (solve-part-1 "day4/input.txt") 200)))))

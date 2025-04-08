(ns advent-of-code-2024.week2.day13-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2024.week2.day13 :refer :all]))

(def day "day13")
(def example "/example.txt")

(deftest solve-part-1-test
  (testing "Solve part 1 with sample data"
    (is (= 480 (solve-part-1 (str day example))))))
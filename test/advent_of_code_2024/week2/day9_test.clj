(ns advent-of-code-2024.week2.day9-test
  (:require [advent-of-code-2024.week2.day9 :refer :all]
            [clojure.test :refer :all]))

(def day "day9")

(deftest solve-part-1-test
  (testing "Solve part 1 using example data"
    (is (= 1928 (solve-part-1 (str day "/example.txt")))))
  (testing "Solve part 1 using input data"
    (is (= 381 (solve-part-1 (str day "/input.txt"))))))


(deftest solve-part-2-test
  (testing "Solve part 2 using example data"
    (is (= 34 (solve-part-2 (str day "/example.txt")))))
  (testing "Solve part 2 using input data"
    (is (= 1184 (solve-part-2 (str day "/input.txt"))))))



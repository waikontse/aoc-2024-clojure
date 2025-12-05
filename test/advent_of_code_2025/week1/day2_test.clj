(ns advent-of-code-2025.week1.day2-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2025.week1.day2 :refer :all]))


(deftest solve-part-1-test
  (testing "Solve part1 with example data"
    (is (= 1227775554 (solve-part-1 example))))
  (testing "Solve part1 with real data"
    (is (= 21898734247 (solve-part-1 input))))
  )

(deftest solve-part-2-test
  (testing "Solve part 2 with example data"
    (is (= 4174379265 (solve-part-2 example))))
  (testing "Solve part 2 with real data"
    (is (= 28915664389 (solve-part-2 input))))
  )

(deftest max-length-spec-test
  (testing "max length spec"
    (is (= 4 (max-length-spec {"lower" 11 "upper" 1141})))))
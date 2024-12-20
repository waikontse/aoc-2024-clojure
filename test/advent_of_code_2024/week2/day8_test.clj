(ns advent-of-code-2024.week2.day8-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2024.week2.day8 :refer :all]))

(deftest solve-part-1-test
  (testing "Solve part 1 using example data"
    (is (= 14 (solve-part-1 "day8/example.txt"))))
  (testing "Solve part 1 using input data"
    (is (= 1620690235709 (solve-part-1 "day8/input.txt"))))
  )


(deftest solve-part-2-test
  (testing "Solve part 2 using example data"
    (is (= 11387 (solve-part-2 "day8/example.txt"))))
  (testing "Solve part 2 using input data"
    (is (= 145397611075341 (solve-part-2 "day8/input.txt"))))
  )

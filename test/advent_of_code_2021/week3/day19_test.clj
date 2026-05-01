(ns advent-of-code-2021.week3.day19-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2021.week3.day19 :refer :all]))

(deftest parse-coordinates-test
  (testing "Parse coordinates"
    (is (= {:x 891 :y -625 :z 532 } (parse-coordinates "891,-625,532")))))

(deftest parse-scanner-line
  (testing "Parse a scanner line with an id"
    (is (= 123 (parse-scanner "--- scanner 123 ---")))))

(ns advent-of-code-2025.week1.day5-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2025.week1.day5 :refer :all]))


(deftest can-merge-range?-test
  (testing "Can merge non-intercepting ranges"
    (is (= false (can-merge-range? {:start 3 :end 8} {:start 9 :end 13}))))
  (testing "Can merge intercepting range"
    (is (= true (can-merge-range? {:start 3 :end 8} {:start 8 :end 13}))))
  (testing "Can merge intercepting range"
    (is (= true (can-merge-range? {:start 3 :end 9} {:start 8 :end 13}))))
  (testing "Can merge intercepting range"
    (is (= true (can-merge-range? {:start 3 :end 13} {:start 8 :end 13}))))
  (testing "Can merge intercepting range"
    (is (= true (can-merge-range? {:start 3 :end 14} {:start 8 :end 13}))))
  (testing "Can merge intercepting range"
    (is (= true (can-merge-range? {:start 3 :end 3} {:start 3 :end 13}))))
  (testing "Can merge intercepting range"
    (is (= true (can-merge-range? {:start 3 :end 11} {:start 3 :end 3}))))
  )

(deftest merge-ranges-test
  (testing "Merge ranges"
    (is (= {:start 3 :end 9} (merge-ranges {:start 3 :end 4 } {:start 4 :end 9}))))
  (testing "Merge ranges"
    (is (= {:start 3 :end 9} (merge-ranges {:start 3 :end 8 } {:start 3 :end 9}))))
  (testing "Merge ranges"
    (is (= {:start 3 :end 9} (merge-ranges {:start 3 :end 9} {:start 4 :end 9}))))
  (testing "Merge ranges"
    (is (= {:start 3 :end 9} (merge-ranges {:start 3 :end 3} {:start 3 :end 9}))))
  (testing "Merge ranges"
    (is (= {:start 3 :end 9} (merge-ranges {:start 3 :end 9} {:start 3 :end 3}))))
  (testing "Merge ranges"
    (is (= {:start 3 :end 11} (merge-ranges {:start 3 :end 11} {:start 5 :end 7}))))
  )
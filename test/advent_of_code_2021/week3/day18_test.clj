(ns advent-of-code-2021.week3.day18-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2021.week3.day18 :refer :all]))

(deftest add-number-test
  (testing "add-number: [[[[4,3],4],4],[7,[[8,4],9]]] + [1,1]")
  (is (= "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
         (add-number "[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]"))))

(deftest magnitude-test
  (testing "magnitude"
    (is (= 143 (magnitude "[[1,2],[[3,4],5]]"))))
  (testing "magnitude"
    (is (= 1384 (magnitude "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))))
  (testing "magnitude"
    (is (= 445 (magnitude "[[[[1,1],[2,2]],[3,3]],[4,4]]"))))
  (testing "magnitude"
    (is (= 791 (magnitude "[[[[3,0],[5,3]],[4,4]],[5,5]]"))))
  (testing "magnitude"
    (is (= 1137 (magnitude "[[[[5,0],[7,4]],[5,5]],[6,6]]"))))
  (testing "magnitude"
    (is (= 3488 (magnitude "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))))
  )




;after addition: [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
;after explode:  [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
;after explode:  [[[[0,7],4],[15,[0,13]]],[1,1]]
;after split:    [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
;after split:    [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
;after explode:  [[[[0,7],4],[[7,8],[6,0]]],[8,1]]


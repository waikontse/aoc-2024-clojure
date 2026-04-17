(ns advent-of-code-2021.week3.day18-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2021.week3.day18 :refer :all]))

(deftest add-number-test
  (testing "add-number: [[[[4,3],4],4],[7,[[8,4],9]]] + [1,1]")
  (is (= "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
         (add-number "[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]"))))

(deftest magnitude-test
  (testing "magnitude"
    (are [input expected] (= expected (magnitude input))
      "[[1,2],[[3,4],5]]" 143
      "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" 1384
      "[[[[1,1],[2,2]],[3,3]],[4,4]]" 445
      "[[[[3,0],[5,3]],[4,4]],[5,5]]" 791
      "[[[[5,0],[7,4]],[5,5]],[6,6]]" 1137
      "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" 3488)))

(deftest split-number-test
  (testing "split examples"
    (are [input expected] (= expected (split-number input))
      "[[[[0,7],4],[15,[0,13]]],[1,1]]" "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"
      "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]" "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")))

(deftest reduce-number-test
  (testing "continuously reduce-number"
    (is (= "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" (reduce-number "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))))
  )

(deftest explode-number-test
  (testing "explode examples"
    (are [input expected]
      (= expected (explode-number input))
      "[[[[[9,8],1],2],3],4]" "[[[[0,9],2],3],4]"
      "[7,[6,[5,[4,[3,2]]]]]" "[7,[6,[5,[7,0]]]]"
      "[[6,[5,[4,[3,2]]]],1]" "[[6,[5,[7,0]]],3]"
      "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
      "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]" "[[[[0,7],4],[15,[0,13]]],[1,1]]"
      )))

(deftest add-number-and-reduce
  (testing
    (is (= "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" (-> (add-number "[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]")
                                                   reduce-number))))
  )

(deftest can-explode-number?-test
  (testing
    (is (= true (can-explode-number? "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))))
  (testing
    (is (= false (can-explode-number? "[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]]"))))
  )

(deftest find-explotable-number-test
  (testing
    (is (= {:match "[2,7]", :start 4, :end 8} (find-explotable-number "[[[[[2,7],4],[9,[10,1]]],[1,1]]]"))))
  (testing
    (is (= {:match "[3,2]", :start 10, :end 14} (find-explotable-number "[[6,[5,[4,[3,2]]]],1]"))))
  )


(deftest solve-part-1-test
  (testing
    (is (= 4289 (solve-part-1 (clojure.string/split-lines input)))))
  (testing
    (is (= 3488 (solve-part-1 (clojure.string/split-lines example)))))
  )

(deftest solve-part-2-test
  (testing
    (is (= 3993 (solve-part-2 example2))))
  (testing
    (is (= 4807 (solve-part-2 input))))
  )


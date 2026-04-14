(ns advent-of-code-2021.week3.day18)

(def example (slurp "./resources/y2021/week3/day18/example.txt"))
(def input (slurp "./resources/y2021/week3/day18/input.txt"))
(def simple-pair-regex #"\[(\d+),(\d+)]")
(def simple-pair-pattern (re-pattern "\\[\\d+,\\d+]"))
(def digits-matcher (re-pattern "\\d+"))

(re-find simple-pair-pattern "[[[[[9,8],1],2],3],4]")
(re-find simple-pair-pattern "[]")

(println example)


(defn add-number
  "docstring"
  [left right]
  (str "[" left "," right "]")
  )

(defn reduce-number
  "docstring"
  [number]
  ;; Explode number until no more explodes are possible, then split number until no more splits are possible, then repeat until no more explodes or splits are possible.
  )

(defn explode-number
  "docstring"
  [number]
  )


(defn can-split-number?
  [number]
  (let [matcher (re-matcher digits-matcher number)
        all-numeric-matches (re-find matcher)
        _ (println "all-numeric-matches: " all-numeric-matches)]
    (some #(>= (Integer/parseInt %) 10) all-numeric-matches)))

(can-split-number? "[[[[0,7],4],[15,[0,13]]],[1,1]]")

(defn split-number
  "docstring"
  [arglist]
  )

(defn find-simple-pair
  "Find the location of the first pair [\\d+,\\d+]. Return the pair and its location in the number."
  [number]
  (let [first-simple-pair (re-find simple-pair-pattern number)]
    first-simple-pair))

(defn magnitude-calc
  "docstring"
  [simple-pair]
  (cond
    (nil? simple-pair) nil
    :else (let [[_, left, right] (re-matches simple-pair-regex simple-pair)]
            (+ (* 3 (Integer/parseInt left)) (* 2 (Integer/parseInt right))))))

(magnitude-calc "[9,1]")

(defn magnitude
  "docstring"
  [number]
  (loop [current-number number]
    (let [first-simple-pair (find-simple-pair current-number)
          pair-value (magnitude-calc first-simple-pair)]
      (cond
        (nil? first-simple-pair) (Integer/parseInt current-number)
        :else (recur (clojure.string/replace-first current-number first-simple-pair pair-value))
        ))))

(magnitude "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]")
;
;Here are a few more magnitude examples:
;
;[[1,2],[[3,4],5]] becomes 143.
;[[[[0,7],4],[[7,8],[6,0]]],[8,1]] becomes 1384.
;[[[[1,1],[2,2]],[3,3]],[4,4]] becomes 445.
;[[[[3,0],[5,3]],[4,4]],[5,5]] becomes 791.
;[[[[5,0],[7,4]],[5,5]],[6,6]] becomes 1137.
;[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]] becomes 3488.



;For example, the final sum of this list is [[[[1,1],[2,2]],[3,3]],[4,4]]:
;
;[1,1]

;[2,2]
;[3,3]
;[4,4]
;The final sum of this list is [[[[3,0],[5,3]],[4,4]],[5,5]]:
;
;[1,1]
;[2,2]
;[3,3]
;[4,4]
;[5,5]
;The final sum of this list is [[[[5,0],[7,4]],[5,5]],[6,6]]:
;
;[1,1]
;[2,2]
;[3,3]
;[4,4]
;[5,5]
;[6,6]
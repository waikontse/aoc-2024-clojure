(ns advent-of-code-2025.week1.day2
  (:require [advent-of-code-2024.utils.io :as io])
  (:require [clojure.pprint :as pprint])
  )

(def example (clojure.string/trim (slurp "./resources/y2025/day2/example.txt")))
(def input (slurp "./resources/y2025/day2/input.txt"))

(defn split-range-into-spec
  [range]
  (let [splitted (clojure.string/split range #"-")
        _ (println "range and splitted" range splitted)
        lower (io/str->int (get splitted 0))
        upper (io/str->int (get splitted 1))
        _ (println "lower and upper" lower upper)
        ]
    {"lower-int" lower "upper-int" upper "lower-str" (get splitted 0) "upper-str" (get splitted 1)})
  )


;; Specs cases
;; even - even -> split str in half and generate and filter
;; even - uneven -> split str in half and generate till next length
;; uneven - even ->
;; uneven - uneven -> ignore



(defn is-within-range?
  ""
  [target upper-bound]
  (<= target upper-bound))


(defn can-skip?
  [spec]
  (let [lower-str (get spec "lower-str")
        upper-str (get spec "upper-str")
        lower-is-uneven? (not (even? (count lower-str)))
        upper-is-uneven? (not (even? (count upper-str)))
        is-same-length? (= (count lower-str) (count upper-str))
        ]
    (and lower-is-uneven? upper-is-uneven? is-same-length?)))

(defn gen-candidates
  [lower-bound upper-bound]
  (range lower-bound (inc upper-bound)))

(gen-candidates 9 23)

(can-skip? (split-range-into-spec "123-1234"))

(defn solve-part-1
  ""
  [input]
  0)

(defn solve-part-2
  ""
  [input]
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def a (clojure.string/split example #","))
(def b (map #(split-range-into-spec %) a))
b
(->> (map #(split-range-into-spec %) a)
     (pprint/pprint))

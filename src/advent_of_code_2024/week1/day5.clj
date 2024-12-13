(ns advent-of-code-2024.week1.day5
  (:require [advent-of-code-2024.utils.io :as io]))

(defn parse-line-to-card
  [raw-line]
  (let [splitted (clojure.string/split raw-line #"\|")
        left-int (Integer/parseInt (first splitted))
        right-int (Integer/parseInt (second splitted))]
    {:left left-int :right right-int}))

(defn parse-lines-to-page-ordering
  [raw-lines]
  (let [line-break (.indexOf raw-lines "")
        page-ordering-lines (subvec raw-lines 0 line-break)
        parsed-ordering-lines (map #(parse-line-to-card %) page-ordering-lines)]
    parsed-ordering-lines))

(defn parse-line-to-update
  "docstring"
  [raw-line]
  (let [splitted (clojure.string/split raw-line #",")]
    (map #(Integer/parseInt %) splitted)))

(defn parse-lines-to-updates
  [raw-lines]
  (let [line-break (.indexOf raw-lines "")
        page-update-lines (subvec raw-lines (inc line-break))
        parsed-update-lines (map #(parse-line-to-update %) page-update-lines)]
    parsed-update-lines))

(defn is-single-update-rule-correct?
  [list-of-page-ordering pages-that-comes-after]
  (let [set-of-page-ordering (set (map :right list-of-page-ordering))
        contains-in-set (map #(contains? set-of-page-ordering %) pages-that-comes-after)]
    (every? identity contains-in-set)))

(defn is-update-rule-correct?
  "docstring"
  [map-of-pages page-number-ordering]
  (let [page-ordering-correct (loop [page-orderings page-number-ordering
                                     acc []]
                                (if (empty? page-orderings)
                                  acc
                                  (recur (rest page-orderings)
                                         (conj acc (is-single-update-rule-correct? (get map-of-pages (first page-orderings)) (rest page-orderings))))
                                  ))]
    (every? identity page-ordering-correct))
  )

(defn are-all-update-rules-correct?
  [map-of-pages page-number-orderings f]
  (loop [acc []
         page-number-orderings page-number-orderings]
    (cond
      (empty? page-number-orderings) acc
      (f (is-update-rule-correct? map-of-pages (first page-number-orderings))) (recur (conj acc (first page-number-orderings))
                                                                                          (rest page-number-orderings))
      :else (recur acc (rest page-number-orderings)))))

(defn get-middle-number
  [numbers]
  (let [middle (quot (count numbers) 2)]
    (nth numbers middle)))

(defn solve-part-1
  [filename]
  (let [raw-lines (io/read-input filename)
        parsed-pages (parse-lines-to-page-ordering raw-lines)
        parsed-updates (parse-lines-to-updates raw-lines)
        page-ordering (group-by :left parsed-pages)
        all-correct-updates (are-all-update-rules-correct? page-ordering parsed-updates true?)
        all-middle-numbers (map #(get-middle-number %) all-correct-updates)]
    (reduce + all-middle-numbers)))


(defn determine-first-correct-order
  [map-of-pages page-number-orderings]
  ; Loop through all the number, until we find the number
  ; that all the numbers that follows are in order.
  (loop [currentIndex 0]
    (let [currentPageValue (nth page-number-orderings currentIndex )
          follow-up-pages (io/vec-remove currentIndex page-number-orderings)
          isOrderCorrect (is-single-update-rule-correct? (get map-of-pages  currentPageValue) follow-up-pages)
          ]
      (cond
        (true? isOrderCorrect) currentPageValue
        (= 0 (count page-number-orderings)) currentPageValue
        :else (recur (inc currentIndex))))))

(defn re-arrange-page-updates
  [map-of-pages page-number-orderings]
  (loop [page-number-orderings page-number-orderings
         length (count page-number-orderings)
         acc []]
    (let [target (when (> length 0) (determine-first-correct-order map-of-pages page-number-orderings))]
      (if (= length 0)
        acc
        (recur (vec (remove #(= target %) page-number-orderings)) (dec length) (conj acc target))))))


(defn solve-part-2
  [filename]
  (let [raw-lines (io/read-input filename)
        parsed-pages (parse-lines-to-page-ordering raw-lines)
        parsed-updates (parse-lines-to-updates raw-lines)
        page-ordering (group-by :left parsed-pages)
        all-correct-updates (are-all-update-rules-correct? page-ordering parsed-updates false?)
        re-arranged-faulty-numbers (map #(re-arrange-page-updates page-ordering (vec %)) all-correct-updates)
        all-middle-numbers (map #(get-middle-number %) re-arranged-faulty-numbers)
        ]
    (reduce + all-middle-numbers)))
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
  (let [splitted (clojure.string/split raw-line #",")
        ;_ (println splitted)
        ]
    (map #(Integer/parseInt %) splitted)
    )
  )

(defn parse-lines-to-updates
  [raw-lines]
  (let [line-break (.indexOf raw-lines "")
        page-update-lines (subvec raw-lines (inc line-break))
        ;_ (println page-update-lines)
        parsed-update-lines (map #(parse-line-to-update %) page-update-lines)
        ;_ (println parsed-update-lines)
        ]
    parsed-update-lines))

(defn is-single-update-rule-correct?
  "docstring"
  [list-of-page-ordering pages-that-comes-after]
  (let [set-of-page-ordering (set (map :right list-of-page-ordering))
        contains-in-set (map #(contains? set-of-page-ordering %) pages-that-comes-after)
        ]
    (every? identity contains-in-set))
  )

(defn is-update-rule-correct?
  "docstring"
  [map-of-pages page-number-ordering]
  (let [page-ordering-correct (loop [page-orderings page-number-ordering
                                     acc []
                                     ]
                                (if (empty? page-orderings)
                                  acc
                                  (recur (rest page-orderings)
                                         (conj acc (is-single-update-rule-correct? (get map-of-pages (first page-orderings)) (rest page-orderings))))
                                  ))
        ]
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
      :else (recur acc (rest page-number-orderings))
      )))

(defn get-middle-number
  [numbers]
  (let [middle (quot (count numbers) 2)]
    (nth numbers middle)))

(defn solve-part-1
  [filename]
  (let [raw-lines (io/read-input filename)
        parsed-pages (parse-lines-to-page-ordering raw-lines)
        parsed-updates (parse-lines-to-updates raw-lines)
        _ (println parsed-pages)
        _ (println parsed-updates)
        page-ordering (group-by :left parsed-pages)
        result (is-update-rule-correct? page-ordering (first parsed-updates))
        _ (println result)
        all-correct-updates (map #(is-update-rule-correct? page-ordering %) parsed-updates)
        _ (println "all corrected: " all-correct-updates)
        all-correct-updates-2 (are-all-update-rules-correct? page-ordering parsed-updates true?)
        _ (println all-correct-updates-2)
        all-middle-numbers (map #(get-middle-number %) all-correct-updates-2)
        _ (println all-middle-numbers)
        ]
    (reduce + all-middle-numbers)))

(defn solve-part-2
  [filename]
  (let [raw-lines (io/read-input "day5/example.txt")
        parsed-pages (parse-lines-to-page-ordering raw-lines)
        parsed-updates (parse-lines-to-updates raw-lines)
        _ (println parsed-pages)
        _ (println parsed-updates)
        page-ordering (group-by :left parsed-pages)
        result (is-update-rule-correct? page-ordering (first parsed-updates))
        _ (println result)
        all-correct-updates (map #(is-update-rule-correct? page-ordering %) parsed-updates)
        _ (println "all corrected: " all-correct-updates)
        all-correct-updates-2 (are-all-update-rules-correct? page-ordering parsed-updates false?)
        _ (println all-correct-updates-2)
        all-middle-numbers (map #(get-middle-number %) all-correct-updates-2)
        _ (println all-middle-numbers)
        ]
    (reduce + all-middle-numbers))
  )


(reduce (fn [acc item]
          (println "received item: " item)
          (conj acc item))
        []
        ["abdc" "ab" "c"])
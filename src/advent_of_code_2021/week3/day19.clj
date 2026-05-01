(ns advent-of-code-2021.week3.day19
  (:require [utils.algorithms :as algo]
            [clojure.math.combinatorics :as combi])
  )

(def example (slurp "resources/y2021/week3/day19/example.txt"))
(def input (slurp "resources/y2021/week3/day19/input.txt"))


(defn parse-scanner
  "Parses 1 raw scanner line E.g. '--- scanner 0 ---'"
  [raw-scanner-line]
  (let [[[_, id]] (re-seq #"--- scanner (\d+) ---" raw-scanner-line)]
    (Integer/parseInt id)))


(defn parse-coordinates
  [raw-coordinates]
  (let [[x-str, y-str, z-str] (clojure.string/split raw-coordinates #",")
        x (Integer/parseInt x-str)
        y (Integer/parseInt y-str)
        z (Integer/parseInt z-str)
        ]
    {:x x :y y :z z}))

(defn parse-input-to-puzzle
  [parsed-raw-input]
  (let [[scanner & coordinates] parsed-raw-input
        scanner-id (parse-scanner scanner)
        coordinates (map #(parse-coordinates %) coordinates)
        ]
    {:id scanner-id :coordinates coordinates}))

(defn parse-input
  "docstring"
  [raw-input]
  (let [coll (reduce (fn [coll item]
                       (cond
                         (clojure.string/blank? item) (-> (update coll :complete conj (:partial coll))
                                                          (update :partial (constantly [])))
                         :else (update coll :partial conj item)))
                     {:partial [] :complete []}
                     (clojure.string/split-lines raw-input))
        completed-coll (conj (:complete coll) (:partial coll))]
    (map #(parse-input-to-puzzle %) completed-coll)))

(defn update-puzzle-with-distances
  [puzzle-input]
  (let [coordinates (:coordinates puzzle-input)
        combinations-coordinates (combi/combinations coordinates 2)
        distances (map (fn [pair]
                         (let [from (first pair)
                               to (second pair)
                               distance (algo/distance-3d from to)]
                           {:from from :to to :distance distance}))
                       combinations-coordinates)
        sorted-distances (sort-by :distance distances)]
    (assoc puzzle-input :distances sorted-distances)))

(defn update-puzzles-with-distances [puzzle-inputs]
  (map #(update-puzzle-with-distances %) puzzle-inputs))

(defn find-all-matching [] nil)

(parse-input example)

(defn solve-part-1 [input]
  (let [parsed-scanners (parse-input input)
        ]
    (update-puzzles-with-distances parsed-scanners))
  )


(solve-part-1 example)
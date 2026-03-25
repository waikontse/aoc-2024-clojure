(ns advent-of-code-2025.week2.day12
  (:require [clojure.string :as str]
            [advent-of-code-2024.utils.board :as board]))

(def example (slurp "./resources/y2025/day12/example.txt"))
(def input (slurp "./resources/y2025/day12/input.txt"))

(defn find-splits
  [raw-lines]
  (reduce (fn [acc line]
            (if (clojure.string/blank? line)
              (let [updated-finished (conj (:finished acc) (:partial acc))
                    updated-acc (assoc acc :partial [] :finished updated-finished)]
                updated-acc)
              (let [updated-partial (conj (:partial acc) line)
                    updated-acc (assoc acc :partial updated-partial)]
                updated-acc)
              ))
          {:partial [] :finished []}
          raw-lines))

(defn parse-raw-blocks
  "Parse block lines lie:
    0: ### ##. ##.
  Returns a sequence of maps {:index ind :board board}"
  [raw-blocks]
  (->> (map #(str/join " " %) raw-blocks)
       (map str/trim)
       (map (fn [line]
              (let [[index block-lines] (str/split line #":" 2)
                    parsed-board (-> block-lines
                                     str/trim
                                     (str/split #"\s+")
                                     (board/parse-to-board))
                    area (count (filter #(= % \#) (:board parsed-board)))]
                {:index (Integer/parseInt index) :board parsed-board :area area})))
       ))

(parse-raw-blocks  [["0:" "###" "##." "##."]])

(defn parse-raw-configs
  "Parse config lines like:
     4x4: 0 0 0 0 2 0
     12x5: 1 0 1 0 2 2
   Returns a sequence of maps {:width w :height h :counts [..]}"
  [raw-configs]
  (->> raw-configs
       (map str/trim)
       (remove str/blank?)
       (map (fn [line]
              (let [[dim counts] (str/split line #":" 2)
                    [_ w h] (re-matches #"(\d+)x(\d+)" (str/trim dim))
                    counts (-> counts
                               str/trim
                               (str/split #"\s+")
                               (->> (map #(Integer/parseInt %))
                                    vec))]
                {:width (Integer/parseInt w)
                 :height (Integer/parseInt h)
                 :counts counts})))
       ))

(defn can-bin-pack-area?
  [puzzle-config mapped-parsed-blocks]
  (let [puzzle-area (* (:width puzzle-config) (:height puzzle-config))
        requested-puzzle-area (->> (:counts puzzle-config)
                                   (map-indexed (fn [index item]
                                                  (->> (get mapped-parsed-blocks index 0)
                                                       (:area)
                                                       (* item))))
                                   (reduce +))
        _ (println "request area" requested-puzzle-area)
        ]
    (<= requested-puzzle-area puzzle-area))
  )

(defn solve-part-1
  [raw-input]
  (let [split-lines (clojure.string/split-lines raw-input)
        split-puzzle (find-splits split-lines)
        _ (println split-puzzle)
        _ (println (count (:partial split-puzzle)))
        parsed-blocks (parse-raw-blocks (:finished split-puzzle))
        mapped-parsed-blocks (zipmap (map #(:index %) parsed-blocks) parsed-blocks)
        _ (clojure.pprint/pprint mapped-parsed-blocks)
        parsed-raw-configs (parse-raw-configs (:partial split-puzzle))
        _ (clojure.pprint/pprint parsed-raw-configs)
        valid-bin-packing-configs (map #(can-bin-pack-area? % mapped-parsed-blocks) parsed-raw-configs)
        _ (println valid-bin-packing-configs)
        ]
    0))

(solve-part-1 example)
;; when loaded in REPL, evaluating `example` will show the raw text; exporting parse helpers above

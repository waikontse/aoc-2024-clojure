(ns advent-of-code-2024.week2.day9
  (:require [advent-of-code-2024.utils.io :as io]
            [clojure.pprint :as pp])
  )

(defn get-symbol-for-index
  [index]
  (if (even? index)
    (/ index 2)
    \.))

(defn calc-free-spaces
  [disk-map]
  (let [free-spaces (map-indexed (fn [idx x]
                                   (if (odd? idx)
                                     (Integer/parseInt (str x))
                                     0))
                                 disk-map)]
    (reduce + free-spaces)))

(defn preprocess-raw-map
  [disk-map]
  (map-indexed (fn [index item]
                 {:id     index
                  :symbol (get-symbol-for-index index)
                  :length (Integer/parseInt (str item))
                  :rest   (Integer/parseInt (str item))
                  })
               disk-map)
  )

(defn split-processed-map
  "Splits the map into 2 sections. 1 for files and 1 for free spaces.
   E.g. {:files {} :free-spaces {} }"
  [processed-map]
  (reduce (fn [coll item]
            (if (even? (:id item))
              (assoc-in coll [:files (:id item)] item)
              (assoc-in coll [:free-spaces (:id item)] item)))
          {:files {} :free-spaces {}}
          processed-map))


(defn expand
  [disk-map]
  (println "mapping disk" disk-map)
  (let [preprocessed-map (preprocess-raw-map disk-map)
        _ (println "expanding pre-processed")
        expanded (reduce (fn [coll item]
                           (into coll (repeat (:length item) (:symbol item))))
                         []
                         preprocessed-map)]
    (vec expanded)))

(defn find-last-non-empty
  "docstring"
  [coll]
  (let [reversed (rseq coll)
        index (first (keep-indexed (fn [idx x]
                                     (when (not= \. x)
                                       idx))
                                   reversed))
        ]
    (- (count coll) index 1)))

(defn compact-once
  "docstring"
  [expanded-disk-map position-to-fill]
  (let [last-position-with-number (find-last-non-empty expanded-disk-map)
        compacted (io/swap-vec expanded-disk-map position-to-fill last-position-with-number)
        ]
    compacted))

(defn compact-fully
  "docstring"
  [expanded-disk-map total-free-space]
  (println "Compacting fully")
  (loop [curr-expanded-disk-map expanded-disk-map]
    (let [curr-free-space-idx (.indexOf curr-expanded-disk-map \.)]
      (if (= curr-free-space-idx (- (count expanded-disk-map) total-free-space))
        curr-expanded-disk-map
        (recur (compact-once curr-expanded-disk-map curr-free-space-idx))))))

(defn checksum
  "docstring"
  [compacted-disk-map]
  (println "Performing checksum")
  (let [multiplied (map-indexed (fn [idx x]
                                  (* idx (if (number? x) x 0)))
                                compacted-disk-map)]
    (reduce + multiplied)))

(defn solve-part-1
  "docstring"
  [filename]
  (let [raw-line (first (io/read-input "day9/example.txt"))
        expanded-raw-disk (expand raw-line)
        total-free-space (calc-free-spaces raw-line)
        compacted (compact-fully expanded-raw-disk total-free-space)
        ;_ (println compacted)
        ]
    (checksum compacted))
  )

(defn split-expanded
  "docstring"
  [expanded-raw-disk]
  (let [files (map-indexed (fn [idx x]
                             (when (even? idx) x))
                           expanded-raw-disk)
        spaces (map-indexed (fn [idx x]
                              (when (odd? idx) x))
                            expanded-raw-disk)
        ]
    {:files (remove nil? files) :spaces (remove nil? spaces)}))

(defn expand-item
  [[k v]]
  [k (assoc v :val [])])

(expand-item [1 {:id 1 :length 5 :symbol \.}])

(defn expand-items
  "Expands items by creating a :val and a string inside the map"
  [coll]
  (let [free-spaces (:free-spaces coll)
        free-spaces-kvs (map #(expand-item %) free-spaces)
        new-free-spaces (into {} free-spaces-kvs)]
    (assoc-in coll [:free-spaces] new-free-spaces)))

(def disk-data {:files
                {0  {:id 0, :symbol 0, :length 2},
                 4  {:id 4, :symbol 2, :length 1},
                 6  {:id 6, :symbol 3, :length 3},
                 12 {:id 12, :symbol 6, :length 4},
                 2  {:id 2, :symbol 1, :length 3},
                 14 {:id 14, :symbol 7, :length 3},
                 16 {:id 16, :symbol 8, :length 4},
                 10 {:id 10, :symbol 5, :length 4},
                 18 {:id 18, :symbol 9, :length 2},
                 8  {:id 8, :symbol 4, :length 2}},
                :free-spaces
                {7  {:id 7, :symbol \., :length 1},
                 1  {:id 1, :symbol \., :length 3},
                 15 {:id 15, :symbol \., :length 1},
                 13 {:id 13, :symbol \., :length 1},
                 17 {:id 17, :symbol \., :length 0},
                 3  {:id 3, :symbol \., :length 3},
                 11 {:id 11, :symbol \., :length 1},
                 9  {:id 9, :symbol \., :length 1},
                 5  {:id 5, :symbol \., :length 3}}})

(expand-items disk-data)


(defn solve-part-2
  "docstring"
  [filename]
  (let [raw-line (first (io/read-input "day9/example.txt"))
        expanded-raw-disk (preprocess-raw-map raw-line)
        ;compacted (compact-fully expanded-raw-disk total-free-space)
        _ (println (split-expanded expanded-raw-disk))
        _ (println "raw expanded")
        _ (println expanded-raw-disk)
        _ (pp/pprint (split-processed-map expanded-raw-disk))
        ]
    0)
  )
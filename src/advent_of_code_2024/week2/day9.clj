(ns advent-of-code-2024.week2.day9
  (:require [advent-of-code-2024.utils.io :as io]))

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
   (->> (map-indexed (fn [index item]
                       ;(println "index" index "item " item)
                 {:symbol (get-symbol-for-index index) :length (Integer/parseInt (str item))})
               disk-map)
        ))

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

(defn solve-part-2
  "docstring"
  [filename]
  (let [raw-line (first (io/read-input "day9/example.txt"))
        expanded-raw-disk (preprocess-raw-map raw-line)
        ;compacted (compact-fully expanded-raw-disk total-free-space)
        _ (println (split-expanded expanded-raw-disk))
        _ (println "raw expanded")
        _ (println expanded-raw-disk)
        ]
    0)
  )
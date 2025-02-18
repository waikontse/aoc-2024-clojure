(ns advent-of-code-2024.week2.day9
  (:require [advent-of-code-2024.utils.io :as io]
            [clojure.pprint :as pp])
  )

(defn get-symbol-for-index
  [index]
  (if (even? index)
    (/ index 2)
    \.))


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

;;
;; Checksum for the revised algorithm
;;
(defn checksum-file
  "make checksum from a file description"
  [file-description startIdx]
  (let [file-range (range startIdx (:length file-description))
        val (:symbol file-description)
        ]
    (* (reduce + file-range) val)))

(defn checksum-compacted-space
  [space-description startIdx]
  (let [space-range (range startIdx (+ startIdx (:length space-description)))
        exploded-chars (seq (:value space-description))
        zipped (map vector space-range exploded-chars)
        ]
    (->> zipped
         (map #(* (first %) (Character/getNumericValue (second %))))
         (reduce +))))

(checksum-file {:symbol 9, :length 4} 0)
(checksum-compacted-space {:symbol \. :value "999" :length 3} 2)


(defn can-fully-fill?
  [file freespace]
  (<= (:length file) (:length freespace)))

(defn compact-disk-map
  "With a map of {:files n :free-spaces x}"
  ([disk-map]
   (compact-disk-map 1 ""))
  ([disk-map free-space-idx rest]
   (let [files-length (count (:files disk-map))
         file-index (* 2 files-length)
         ]
     (if (<= file-index free-space-idx)
       disk-map
       (recur )
       ;; else, fill in the free-space)
     ;; stop condition
       )
     )
   )
  )

;;
;;
;;
(defn finalize-freespace
  "Finalize a freespace record. {:id id :symbol \\. :length n :rest n :val}"
  [freespace]
  (let [combined-val (apply str (apply concat (:val freespace)))
        combined-val-length (count combined-val)
        remaining (- (:length freespace) combined-val-length)
        new-val (cond
                  (empty? (:val freespace)) (apply str (repeat (:length freespace) 0))
                  (= combined-val-length (:length freespace)) combined-val
                  (< remaining 0) (throw (Exception. "Error finalizing freespace."))
                  :else (str combined-val (apply str (repeat remaining 0))))
        ]
    (assoc-in freespace [:val] new-val)))

(finalize-freespace {:id 1 :symbol \. :length 5 :val []})
(finalize-freespace {:id 1 :symbol \. :length 5 :val [["99999"]]})
(finalize-freespace {:id 1 :symbol \. :length 5 :val [["111"] ["2"]]})

(defn combine-files-and-freespace
  "Expecting {:files n :free-spaces x}"
  [disk-map]
  (let [finalized-freespaces (map #(finalize-freespace %) (:free-spaces disk-map))
        ]
    {:files (conj (:files disk-map) finalized-freespaces)}))

;; (combine-files-and-freespace (expand-items disk-data))

;;
;; End of test functions
;;


(defn solve-part-1
  "docstring"
  [filename]
  (let [raw-line (first (io/read-input "day9/example.txt"))
        expanded-raw-disk (preprocess-raw-map raw-line)
        _ (println "raw expanded")
        data (->> expanded-raw-disk
                  split-processed-map
                  ;; TODO implement compaction
                  expand-freespace-items
                  ;; TODO implement check summing)
        _ (pp/pprint data)
        ]
    0)
  )


(defn expand-freespace-item
  [[k v]]
  [k (assoc v :val [])])

(defn expand-freespace-items
  "Expands items by creating a :val and a string inside the map"
  [coll]
  (let [free-spaces (:free-spaces coll)
        free-spaces-kvs (map #(expand-freespace-item %) free-spaces)
        new-free-spaces (into {} free-spaces-kvs)]
    (assoc-in coll [:free-spaces] new-free-spaces)))

(def disk-data
  {:files
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

(expand-freespace-items disk-data)


(defn solve-part-2
  "docstring"
  [filename]
  (let [raw-line (first (io/read-input "day9/example.txt"))
        expanded-raw-disk (preprocess-raw-map raw-line)
        _ (println "raw expanded")
        data (->> expanded-raw-disk
                  split-processed-map
                  expand-freespace-items)
        _ (pp/pprint data)
        ]
    0)
  )

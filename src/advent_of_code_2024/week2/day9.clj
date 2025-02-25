(ns advent-of-code-2024.week2.day9
  (:require [advent-of-code-2024.utils.io :as io]
            [clojure.pprint :as pp]))

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
                  :rest   (Integer/parseInt (str item))})
               disk-map))

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

(defn determine-next-free-space-node
  [disk-map start-free-space-idx file-length]
  (printf "determining next free space. Start-index: %d length: %d %n" start-free-space-idx file-length)
  (loop [free-space-idx start-free-space-idx
         total-free-space (get-in disk-map [:free-spaces start-free-space-idx :rest])]
    (if (>= total-free-space file-length)
      free-space-idx
      (recur (+ free-space-idx 2)
             (+ total-free-space (get-in disk-map [:free-spaces (+ free-space-idx 2) :rest]))))))

(defn fill-space
  [disk-map free-space-id data]
  (let [free-space-val (get-in disk-map [:free-spaces free-space-id :val])
        new-free-space-val (into free-space-val data)
        new-rest (- (get-in disk-map [:free-spaces free-space-id :rest]) (count data))
        new-disk-map (->
                      (assoc-in disk-map [:free-spaces free-space-id :val] new-free-space-val)
                      (assoc-in [:free-spaces free-space-id :rest] new-rest))
        ;;_ (printf "fill space: data: %s id: %d new-free-space-val: %s new-rest: %d%n" data free-space-id new-free-space-val new-rest)
        ;; _ (pp/pprint new-disk-map)
        ]
    new-disk-map))

(defn fill-spaces-upto
  [disk-map free-space-from free-space-to data]
  (loop [updated-map disk-map
         current-free-space-idx free-space-from
         rest-data-to-fill data]
    (let [free-space-length-for-index (get-in updated-map [:free-spaces current-free-space-idx :rest])
          data-to-fill (take free-space-length-for-index data)
          ;; _ (println "fill-spaces-upto: " free-space-from free-space-to)
          ;; _ (println "fill-spaces-upto: " free-space-length-for-index data-to-fill)
          ]
      (if (> current-free-space-idx free-space-to)
        updated-map
        (recur (fill-space updated-map current-free-space-idx data-to-fill)
               (+ current-free-space-idx 2)
               (drop free-space-length-for-index data))))))

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

(defn compact-disk-map
  "With a map of {:files n :free-spaces x}"
  ([disk-map] (compact-disk-map disk-map 1))
  ([disk-map free-space-idx]
   (let [files-length (count (:files disk-map))
         file-index (* 2 (dec files-length))
         ;; _ (println "File index and free-space node: " file-index free-space-idx)
         ]
     (if (<= file-index free-space-idx)
       disk-map
       (let [current-file-to-compact (get-in disk-map [:files file-index])
             file-length (:length current-file-to-compact)
             ;; TODO update dat to be filled
             data-to-be-filled (repeat file-length (:symbol current-file-to-compact))
             last-free-space-index (determine-next-free-space-node disk-map free-space-idx file-length)
             filled-disk-map (fill-spaces-upto disk-map free-space-idx last-free-space-index data-to-be-filled)]
         (recur (update-in filled-disk-map [:files] dissoc file-index) last-free-space-index))))))

;;
;;
;;
(defn finalize-freespace
  "Finalize a freespace record. {:id id :symbol \\. :length n :rest n :val}"
  [[k v]]
  (let [combined-val (:val v)
        combined-val-length (count combined-val)
        remaining (- (:length v) combined-val-length)
        new-val (cond
                  (empty? (:val v)) (into (repeat (:length v) 0))
                  (= combined-val-length (:length v)) combined-val
                  (< remaining 0) (throw (Exception. "Error finalizing freespace."))
                  :else (into combined-val (repeat remaining 0)))]
    (vector k (assoc-in v [:val] new-val))))

(finalize-freespace [1 {:id 1 :symbol \. :length 5 :val []}])
(finalize-freespace [2 {:id 1 :symbol \. :length 5 :val [1 1 3 3 3]}])
(finalize-freespace [3 {:id 1 :symbol \. :length 5 :val [1 1]}])

(defn combine-files-and-freespace
  "Expecting {:files n :free-spaces x}"
  [disk-map]
  (let [finalized-freespaces (map #(finalize-freespace %) (:free-spaces disk-map))]
    {:files (merge (:files disk-map) (into {} finalized-freespaces))}))

;;
;; Checksum for the revised algorithm
;;
(defn checksum-file
  "make checksum from a file description"
  [file-description startIdx]
  (let [file-range (range startIdx (+ startIdx (:length file-description)))
        val (:symbol file-description)]
    (* (reduce + file-range) val)))

(defn append-space-value-if-missing
  [expected-length value]
  (if (= (count value) expected-length)
    value
    (let [missing-length (- expected-length (count value))
          padding (repeat missing-length 0)]
      (into value padding))))

(defn checksum-compacted-space
  [space-description startIdx]
  (let [space-range (range startIdx (+ startIdx (:length space-description)))
        padded-vals (append-space-value-if-missing (:length space-description)
                                                   (:val space-description))
        zipped (map vector space-range padded-vals)]
    (->> zipped
         (map #(* (first %) (second %)))
         (reduce +))))

(defn checksum-node
  [node start-idx]
  (if (= (:symbol node) \.)
    (checksum-compacted-space node start-idx)
    (checksum-file node start-idx)))

(checksum-file {:symbol 99, :length 4} 1)
(checksum-compacted-space {:symbol \. :val [9 9 9] :length 3} 0)
(checksum-compacted-space {:symbol \. :val [9] :length 1} 1)

(defn summation
  [disk-map]
  (let [sorted-keys (sort (keys (:files disk-map)))
        files (:files disk-map)]
    (loop [acc 0
           curr-idx 0
           keys sorted-keys]
      (printf "Current acc: %d curr-idx: %d%n" acc curr-idx)
      (if (empty? keys)
        acc
        (recur (+ acc (checksum-node (get files (first keys)) curr-idx))
               (+ curr-idx (get-in files [(first keys) :length]))
               (rest keys))))))

(defn filter-out-empty-nodes
  [m]
  (let [pred (fn [[k v]]
               (let [filtered (not= 0 (:length v))
                     _ (printf "%nKey: %s Val: %s%n" k v)
                     _ (println "is filtered: " filtered)
                     ]
                 filtered)
               )
        filtered-files (into {} (filter pred (:files m)))
        filtered-spaces (into {} (filter pred (:free-spaces m)))
        ;; _ (println "filtered files" filtered-files )
        ;; _ (println "filtered spces"  filtered-spaces)
        ]
    {:files filtered-files :free-spaces filtered-spaces})
  )

;; (filter-out-empty-nodes {:files {0 {:length 1}}
;;                          :free-spaces {1 {:length 2}}
;;                          })

(defn solve-part-1
  "docstring"
  [filename]
  (let [raw-line (first (io/read-input "day9/example3.txt"))
        expanded-raw-disk (preprocess-raw-map raw-line)
        _ (println "raw expanded")
        data (->> expanded-raw-disk
                  split-processed-map
                  ;; filter-out-empty-nodes
                  expand-freespace-items
                  compact-disk-map
                  filter-out-empty-nodes
                  combine-files-and-freespace)
        filtered-data (->> expanded-raw-disk
                           split-processed-map
                           filter-out-empty-nodes)
        _ (pp/pprint expanded-raw-disk)
        _ (pp/pprint data)
        _ (pp/pprint filtered-data)
        sum (summation data)
        ]
    sum)

  ;; Answer for input: 6279058075753
  )

(defn solve-part-2
  "docstring"
  [filename]
  (let [raw-line (first (io/read-input "day9/example.txt"))
        expanded-raw-disk (preprocess-raw-map raw-line)
        _ (println "raw expanded")
        data (->> expanded-raw-disk
                  split-processed-map
                  expand-freespace-items)
        _ (pp/pprint data)]
    0))

;; demo result: 2858

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

;(expand-freespace-items disk-data)

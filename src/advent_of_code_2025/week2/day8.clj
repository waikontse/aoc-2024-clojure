(ns advent-of-code-2025.week2.day8
  (:require [advent-of-code-2024.utils.io :as io]))

(def example (slurp "./resources/y2025/day8/example.txt"))
(def input (slurp "./resources/y2025/day8/input.txt"))

(defn calc-distance
  [point-from point-to]
  (let [diff-x (- (:x point-from) (:x point-to))
        diff-y (- (:y point-from) (:y point-to))
        diff-z (- (:z point-from) (:z point-to))
        total-dist (Math/sqrt (+ (* diff-z diff-z) (* diff-x diff-x) (* diff-y diff-y)))]
    {:from point-from :to point-to :distance total-dist}))

(defn calc-distance-all-pairs
  [coll]
  (map-indexed (fn [idx item]
                 (let [rest (drop (inc idx) coll)
                       all-distances (map #(calc-distance item %) rest)]
                   all-distances))
               coll))

(defn merge-circuits
  "Merge all the nodes connect to the to node, to the from node."
  [from to circuits seen]
  (let [from-connected-to (get seen from)
        to-connected-to (get seen to)
        from-circuit (get circuits from-connected-to)
        to-circuit (get circuits to-connected-to)
        combined-circuit (conj (into from-circuit to-circuit) to-connected-to)
        updated-circuits (-> (assoc circuits from-connected-to combined-circuit)
                             (dissoc to-connected-to))
        updated-seen (-> (reduce (fn [coll item] (assoc coll item from-connected-to))
                                 seen
                                 to-circuit)
                         (assoc to from-connected-to to-connected-to from-connected-to))]
    {:updated-circuits updated-circuits :updated-seen updated-seen}))

(defn connect-junction-boxes
  [coll max-pairs nodes]
  (loop [prioritized-dist (take max-pairs coll)
         seen {}
         circuits {}
         last-connection {}]
    (cond
      (empty? prioritized-dist) (do (println "Finishes connecting all pairs of" max-pairs "seen" (count seen))
                                    circuits)
      (and (= (count seen) nodes) (= 1 (count circuits))) (do
                                                            (println "finished connecting all." last-connection)
                                                            last-connection)
      :else
      (let [curr (first prioritized-dist)
            from (:from curr)
            to (:to curr)
            from-is-seen? (contains? seen from)
            to-is-seen? (contains? seen to)
            from-connected-to (get seen from)
            to-connected-to (get seen to)
            _ (println "seen:" (count seen))]
        (cond
          ;; :from and to are both connected to the same circuits -> continue without doing anything
          (and from-is-seen? to-is-seen? (= from-connected-to to-connected-to)) (do
                                                                                  (println "**skipping because both seen same**" from to)
                                                                                  (recur
                                                                                   (rest prioritized-dist)
                                                                                   seen
                                                                                   circuits
                                                                                   {}))
          ;; :from and to is connected on different circuits -> merge the 2 different circuits
          (and from-is-seen? to-is-seen? (not= from-connected-to to-connected-to)) (do
                                                                                     (println "**Merging 2 connected boxes**" from to)
                                                                                     (let [updated-vals (merge-circuits from to circuits seen)]
                                                                                       (recur
                                                                                        (rest prioritized-dist)
                                                                                        (:updated-seen updated-vals)
                                                                                        (:updated-circuits updated-vals)
                                                                                        {:from from :to to})))
          ;; Connect when :from is connected, and :to is unconnected -> connect to from
          (and from-is-seen? (false? to-is-seen?)) (recur
                                                    (rest prioritized-dist)
                                                    (assoc seen to from-connected-to)
                                                    (assoc circuits from-connected-to (conj (get circuits from-connected-to) to))
                                                    {:from from :to to})
          ;; Connect when :from is unconnected, and to is connected
          (and (false? from-is-seen?) to-is-seen?) (recur
                                                    (rest prioritized-dist)
                                                    (assoc seen from to-connected-to)
                                                    (assoc circuits to-connected-to (conj (get circuits to-connected-to) from))
                                                    {:from from :to to})

          ;; Connect when :from is unconnected, and :to is unconnected
          (and (false? from-is-seen?) (false? to-is-seen?)) (recur
                                                             (rest prioritized-dist)
                                                             (assoc seen from from to from)
                                                             (assoc circuits from (conj (get circuits from) to))
                                                             {:from from :to to}))))))

(defn parse-line-to-point
  [raw-line]
  (let [splitted (clojure.string/split raw-line #",")
        [x y z] (io/strs->ints splitted)]
    {:x x :y y :z z}))

(defn parse-lines-to-points
  [raw-lines]
  (map #(parse-line-to-point %) raw-lines))

(defn solve-part-1
  [input max-pairs nodes]
  (let [raw-lines (clojure.string/split-lines input)
        points (parse-lines-to-points raw-lines)
        all-distances (calc-distance-all-pairs points)
        all-distances-combined (flatten all-distances)
        sorted-all-distances-combined (sort-by #(:distance %) all-distances-combined)
        connected (connect-junction-boxes sorted-all-distances-combined max-pairs nodes)
        ;_ (clojure.pprint/pprint connected)
        result (->> (vals connected)
                    (map #(inc (count %)))
                    (sort)
                    (reverse)
                    (take 3))
        _ (println "result" result)]
    (apply * result)))

(solve-part-1 input 1000000 1000)
;; 6095621910

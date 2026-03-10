(ns advent-of-code-2025.week2.day11
  (:require [clojure.pprint :as pp]))

(def example (slurp "./resources/y2025/day11/example.txt"))
(def example2 (slurp "./resources/y2025/day11/example2.txt"))
(def input (slurp "./resources/y2025/day11/input.txt"))
(def START "you")
(def OUT "out")
(def FFT "fft")
(def DAC "dac")


(defn parse-line
  "Returns: { :name \"...\", :out #{ ... } }"
  [raw-line]
  (let [splits (clojure.string/split raw-line #" ")
        name (clojure.string/replace (first splits) ":" "")
        outputs (map #(clojure.string/trim %) (rest splits))]
    {:name name :out (into #{} outputs)}))

(defn parse-lines
  [raw-lines]
  (map #(parse-line %) raw-lines))

(defn to-map
  [mapxs]
  (into {}
        (for [{:keys [name], :as item} mapxs]
          [name item])))

(defn node-has-target?
  [device target]
  (contains? (:out device) target))

(defn has-out-node?
  [device]
  (node-has-target? device OUT))

(defn count-exits
  [graph start]
  (cond
    (has-out-node? (get graph start)) 1
    :else
    (let [outgoing-paths (:out (get graph start))
          _ (println "looking at:" start "outgoing paths:" outgoing-paths)
          all-outgoing-out-counts (map #(count-exits graph %) outgoing-paths)
          _ (println "all counts:" all-outgoing-out-counts)]
      (reduce + all-outgoing-out-counts))))

(def mem-count-exits (memoize count-exits))

(defn solve-part-1
  [input]
  (let [raw-lines (clojure.string/split-lines input)
        parsed-lines (parse-lines raw-lines)
        ;_ (pp/pprint parsed-lines)
        connected-graph (to-map parsed-lines)
        ;_ (pp/pprint connected-graph)
        ]
    (count-exits connected-graph START)))

;(time (solve-part-1 input))


;;;;;;;;; Solve part 2
(defn find-all-nodes-containing-target
  [graph target]
  (->> (filter #(node-has-target? % target) (vals graph))
       (into [])))

(defn find-all-parents
  [graph childs]
  (let [                                                    ;_ (println "childs:" childs)
        parents (flatten (map #(find-all-nodes-containing-target graph %) childs))
        ;_ (println "parents" parents)
        ]
    (map #(:name %) parents)))


(defn perculate-up-to-find-all-ancestors
  "Find all roots that can lead to the target node"
  [graph target]
  (let [current-nodes (find-all-nodes-containing-target graph target)
        out-nodes-of-target (conj (:out (get graph target)) target)
        ;_ (println "current nodes:" current-nodes)
        ]
    (loop [current-targets (map #(:name %) current-nodes)
           collected (-> (map #(:name %) current-nodes)
                          (set)
                          (into out-nodes-of-target))
           ]
      (cond
        (nil? (first current-targets)) collected
        :else
        ;; filter out loops
        (let [all-parents (find-all-parents graph current-targets)
              ;_ (println "found parents:" all-parents)
              non-seen-ancestors (set (filter #(not (contains? collected %)) all-parents))
              updated-seen (into collected non-seen-ancestors)]
          (recur non-seen-ancestors updated-seen)))
      )
    )
  )


(def example-graph (->> (clojure.string/split-lines example2)
                        (parse-lines)
                        (to-map)))
(def input-graph (->> (clojure.string/split-lines input)
                        (parse-lines)
                        (to-map)))

(find-all-nodes-containing-target example-graph OUT)
(perculate-up-to-find-all-ancestors example-graph "fft")
;; 412 items for DAC, 101 items for fft

(find-all-nodes-containing-target example-graph "aaa")
(find-all-parents example-graph ["aaa" "dac"])

;; Steps to solve part 2
; 1. Find all nodes that can lead to fft, including outs of fft
; 2. Find all nodes that can lead to dac, including outs of dac
; 3. Find all the out nodes
; 4. Combines all the nodes and use that as the "allow" list

;; 1. First follow fft nodes, afterwards dac nodes
;; 2. First follow dac nodes, afterwards fft nodes

(defn contains-fft-and-dac?
  [seen-set]
  (and (contains? seen-set FFT)
       (contains? seen-set DAC)))

(defn count-exits-part-2
  [graph start first-set first-node-name second-set seen]
  (println "looking at start:" start)
  (cond
    (contains-fft-and-dac? seen) (dorun (println "-1") (count-exits graph start))
    (contains? seen start) (dorun (println "0") 0)

    ;; has seen 1st node, but current node does not lead to the second node
    (and (contains? seen first-node-name) (not (contains? second-set start))) (dorun (println "1") 0)

    ;; has seen 1st node, and current node does lead to the second node
    (and (contains? seen first-node-name) (contains? second-set start))
    #_=> (dorun
           (println "3")
           (let [outgoing-paths (:out (get graph start))
                 allowed-outgoing-paths (filter #(contains? second-set %) outgoing-paths)
                 _ (println "outgoing" outgoing-paths "allowed-outgoing" allowed-outgoing-paths "seen" seen)
                 updated-seen (conj seen start)]
             (map #(count-exits-part-2 graph % first-set first-node-name second-set updated-seen) allowed-outgoing-paths)))

    ;; has not seen 1st node, and current node does not lead to the first node
    (and (not (contains? seen first-node-name)) (not (contains? first-set start))) (dorun (println "2") 0)

    ;; has not seen 1st node, and current node does lead to the first node
    (and (not (contains? seen first-node-name)) (contains? first-set start))
    #_=> (dorun
           (println "4")
           (let [outgoing-paths (:out (get graph start))
                 allowed-outgoing-paths (filter #(contains? first-set %) outgoing-paths)
                 _ (println "outgoing" outgoing-paths "allowed-outgoing" allowed-outgoing-paths "seen" seen)
                 updated-seen (conj seen start)]
             (map #(count-exits-part-2 graph % first-set first-node-name second-set updated-seen) allowed-outgoing-paths)))
    :else (println "ERROR")))

(defn solve-part-2
  [input]
  (let [raw-lines (clojure.string/split-lines input)
        parsed-lines (parse-lines raw-lines)
        ;_ (pp/pprint parsed-lines)
        connected-graph (to-map parsed-lines)
        fft-nodes (perculate-up-to-find-all-ancestors connected-graph "fft")
        dac-nodes (perculate-up-to-find-all-ancestors connected-graph "dac")
        result-fft-dac (count-exits-part-2 connected-graph "svr" fft-nodes FFT dac-nodes #{})
        result-dac-fft (count-exits-part-2 connected-graph "svr" dac-nodes DAC fft-nodes #{})
        ]
    (+ (reduce + result-fft-dac) (reduce + result-dac-fft))))

(time (solve-part-2 example2))

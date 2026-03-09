(ns advent-of-code-2025.week2.day11
  (:require [clojure.pprint :as pp]))

(def example (slurp "./resources/y2025/day11/example.txt"))
(def example2 (slurp "./resources/y2025/day11/example2.txt"))
(def input (slurp "./resources/y2025/day11/input.txt"))
(def START "you")
(def OUT "out")


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
          all-outgoing-out-counts (map #(count-exits graph %) outgoing-paths)]
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
        out-nodes-of-target (:out (get graph target))
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
(perculate-up-to-find-all-ancestors input-graph "fft")      ;; 412 items for DAC, 101 items for fft

(find-all-nodes-containing-target example-graph "aaa")
(find-all-parents example-graph ["aaa" "dac"])

;; Steps to solve part 2
; 1. Find all nodes that can lead to fft, including outs of fft
; 2. Find all nodes that can lead to dac, including outs of dac
; 3. Find all the out nodes
; 4. Combines all the nodes and use that as the "allow" list

;; 1. First follow fft nodes, afterwards dac nodes
;; 2. First follow dac nodes, afterwards fft nodes


(defn solve-part-2
  [input]
  (let [raw-lines (clojure.string/split-lines input)
        parsed-lines (parse-lines raw-lines)
        ;_ (pp/pprint parsed-lines)
        connected-graph (to-map parsed-lines)
        ;_ (pp/pprint connected-graph)
        ]
    (count-exits connected-graph "svr")))

;(time (solve-part-2 input))

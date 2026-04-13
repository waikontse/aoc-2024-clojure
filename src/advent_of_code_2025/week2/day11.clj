(ns advent-of-code-2025.week2.day11
  (:require [clojure.pprint :as pp]
            [clojure.set :refer [difference]]
            )
  )

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
  [graph start seen]
  (cond
    (has-out-node? (get graph start)) 1
    :else
    (let [outgoing-paths (:out (get graph start))
          filtered-outgoing-paths (filter #(not (contains? seen %)) outgoing-paths)
          _ (if (not= (count outgoing-paths) (count filtered-outgoing-paths)) (println "loop found.") nil)
          updated-seen (conj seen start)
          all-outgoing-out-counts (map #(count-exits graph % updated-seen) filtered-outgoing-paths)
          ]
      (reduce + all-outgoing-out-counts))))


(defn solve-part-1
  [input start-node]
  (let [raw-lines (clojure.string/split-lines input)
        parsed-lines (parse-lines raw-lines)
        ;_ (pp/pprint parsed-lines)
        connected-graph (to-map parsed-lines)
        ;_ (pp/pprint connected-graph)
        ]
    (count-exits connected-graph start-node #{})))

(time (solve-part-1 input "dac"))

;;;;;;;;; Solve part 2
(defn find-all-nodes-containing-target
  [graph target]
  (->> (filter #(node-has-target? % target) (vals graph))
       (into [])))

(defn find-all-parents
  [graph childs]
  (let [parents (flatten (map #(find-all-nodes-containing-target graph %) childs))
        ]
    (map #(:name %) parents)))

(defn percolate-up-to-find-all-ancestors
  "Find all roots that can lead to the target node"
  [graph target]
  (let [current-nodes (find-all-nodes-containing-target graph target)
        out-nodes-of-target (conj (:out (get graph target)) target)
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
              non-seen-ancestors (set (filter #(not (contains? collected %)) all-parents))
              updated-seen (into collected non-seen-ancestors)]
          (recur non-seen-ancestors updated-seen))))))

(defn count-path-from-to
  [graph start-node end-node seen allowed-list]
  (cond
    (= end-node start-node) 1
    :else
    (let [outgoing-paths (:out (get graph start-node))
          filtered-outgoing-paths (filter #(not (contains? seen %)) outgoing-paths)
          filtered-outgoing-paths (filter #(contains? allowed-list %) filtered-outgoing-paths)
          updated-seen (conj seen start-node)
          all-filtered-outgoing-paths (map #(count-path-from-to graph % end-node updated-seen allowed-list) filtered-outgoing-paths)
          ]
      (reduce + all-filtered-outgoing-paths))))

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
        connected-graph (to-map parsed-lines)
        allowed-list-to-fft (disj (percolate-up-to-find-all-ancestors connected-graph FFT) DAC)
        allowed-list-to-dac (disj (percolate-up-to-find-all-ancestors connected-graph DAC) FFT)
        svr-to-fft (count-path-from-to connected-graph START FFT #{} allowed-list-to-fft)
        fft-to-dac (count-path-from-to connected-graph FFT DAC #{} (conj (difference allowed-list-to-dac allowed-list-to-fft ) "dqc" "gbl"))
        dac-to-out (count-exits connected-graph DAC #{})
        ]
    (* svr-to-fft fft-to-dac dac-to-out)))

(time (solve-part-2 example2))

(def input-graph (->> (clojure.string/split-lines input)
                      (parse-lines)
                      (to-map)))

;; TODO - Optimize counts from fft to dac.
(count-path-from-to input-graph FFT DAC #{} #{})

;allowed-list-to-fft
; svr ->  fft 3418
; dac -> out 4522
; fft -> dac : 24.420.774
; total: 377.452.269.415.704

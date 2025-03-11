(ns advent-of-code-2024.utils.algorithms
  (:require [advent-of-code-2024.utils.board :as board]))

(def not-contains? (complement contains?))

(defn next-flood-fill-steps
  "Determine the nex possible steps for the flood-fill algorithm"
  [board curr-pos seen]
  (let [left-pos  (-> (board/is-same-symbol-left? board curr-pos)
                      (when (board/get-pos-left curr-pos)))
        right-pos (-> (board/is-same-symbol-right? board curr-pos)
                      (when (board/get-pos-right curr-pos)))
        top-pos (-> (board/is-same-symbol-top? board curr-pos)
                    (when (board/get-pos-top curr-pos)))
        bottom-pos (-> (board/is-same-symbol-bottom? board curr-pos)
                       (when (board/get-pos-bottom curr-pos)))
        ]
    (->> [left-pos right-pos top-pos bottom-pos]
         (filter some?)
         (filter #(not-contains? seen %))))
  )

(defn flood-fill
  "Try to run a flood-fill algorithm given a board and a starting position.
    The algorithm will use the same value as the one found on th board in the
    position.

  Returns a set of position(s) "
  ([board start-pos] (flood-fill board [start-pos] #{}))
  ([board start-pos seen]
   (loop [currently-seen seen
          current-to-visit start-pos
          ]
     (if (empty? current-to-visit)
       currently-seen
       (let [next-steps (next-flood-fill-steps board (first current-to-visit) currently-seen)
             updated-seen (conj currently-seen (first current-to-visit))
             ;; _ (println "updated seen:" updated-seen)
             ]
         (recur updated-seen (into (rest current-to-visit) next-steps)))))
   ))

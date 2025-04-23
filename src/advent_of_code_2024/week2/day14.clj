(ns advent-of-code-2024.week2.day14
  (:require [advent-of-code-2024.utils.io :as io]
            [clojure.pprint :as pp]
            [advent-of-code-2024.utils.algorithms :as algo]
            [advent-of-code-2024.utils.board :as b]
            )
  )

(def pOrV #"(p\=|v\=)")

(defn clean-string
  [line]
  (clojure.string/replace line pOrV ""))

(defn splitted-to-pair
  "E.g. 9,4 -> {:x 9 :y 4}"
  [raw-splitted]
  (let [raw-numbers (clojure.string/split raw-splitted #",")
        left (io/str->int (first raw-numbers))
        right (io/str->int (second raw-numbers))]
    [left right]))

(defn parse-to-spec
  [raw-line]
  (let [clean-line (clean-string raw-line)
        splitted (clojure.string/split clean-line #" ")
        position (splitted-to-pair (first splitted))
        velocity (splitted-to-pair (second splitted))
        ]
    {:position position :velocity velocity})
  )

;; multiply the position by velocity
;; normalize to the grid size
;; partition to the quadrants

(defn multiply-position
  [{:keys [position velocity]}
   multiplier]
  (let [new-velocity (algo/row-multiply-by velocity multiplier)
        new-values (map vector position new-velocity)
        ]
    (map #(apply + %) new-values))
  )

(defn normalize-position
  "Normalizes a position."
  [position
   {:keys [width height]}
   ]
  [(mod (first position) width) (mod (second position) height)]
  )


(def uneven? (complement even?))

(defn is-middle-helper
  [x-or-y width-or-height]
  (if (even? width-or-height)
    false
    (let [middle (/ (dec width-or-height) 2)]
      (if (= middle x-or-y)
        true
        false))
    ))


(defn is-middle?
  [position
   {:keys [width height]}]
  (let [is-width-middle? (is-middle-helper (first position) width)
        is-height-middle? (is-middle-helper (second position) height)]
    (or is-width-middle? is-height-middle?))
  )

;; is middle in width
(is-middle? [4 3] {:width 7 :height 11})

;; is middle in height
(is-middle? [3 7] {:width 7 :height 11})



(defn is-left-side?
  [position
   {:keys [width height]
    :as size}]
  (if (true? (is-middle? position size))
    false
    (let [middle (/ width 2)]
      (<= (first position) middle)))
  )

(defn is-upper-side?
  [position
   {:keys [width height]
    :as size}]
  (if (true? (is-middle? position size))
    false
    (let [middle (/ height 2)]
      (<= (second position) middle))))


(defn split-into-quadrants
  [position
   {:keys [width height]
    :as size}]
  (let [on-middle? (is-middle? position size)
        on-left? (is-left-side? position size)
        on-upper? (is-upper-side? position size)]
    (cond
      (true? on-middle?) :M
      (and (true? on-left?) (true? on-upper?)) :LU
      (and (true? on-left?) (false? on-upper?)) :LL
      (and (false? on-left?) (true? on-upper?)) :RU
      (and (false? on-left?) (false? on-upper?)) :RL
      ))
  )

;; split into qadrants
;; M
(split-into-quadrants [4 3] {:width 11 :height 7})
(split-into-quadrants [3 6] {:width 11 :height 7})
;; LU
(split-into-quadrants [3 5] {:width 11 :height 7})
;; LL
(split-into-quadrants [3 7] {:width 11 :height 7})
;; RU
(split-into-quadrants [5 5] {:width 11 :height 7})
;; RL
(split-into-quadrants [5 7] {:width 11 :height 7})
(split-into-quadrants [5 4] {:width 11 :height 7})
(split-into-quadrants [1 3] {:width 11 :height 7})



(defn calc-safety-factor
  [quadrants]
  (let [allowed-quadrants (filter #(not= :M %) quadrants)
        grouped-quadrants (group-by identity allowed-quadrants)
        LU (count (:LU grouped-quadrants))
        RU (count (:RU grouped-quadrants))
        LL (count (:LL grouped-quadrants))
        RL (count (:RL grouped-quadrants))
        ]
    (* LU RU LL RL)))

(defn solve-part-1
  [filename]
  (let [lines (io/read-input "day14/input.txt")
        ;;size {:width 11 :height 7}
        size {:width 101 :height 103}
        cleaned-lines (map #(clean-string %) lines)
        parsed-lines (map #(parse-to-spec %) cleaned-lines)
        all-new-values (map #(multiply-position % 100) parsed-lines)
        all-normalized (map #(normalize-position % size) all-new-values)
        quadrants (map #(split-into-quadrants % size) all-normalized)
        sum (calc-safety-factor quadrants)
        _ (println sum)
        ]
    sum)
  )




(defn is-easter-egg2?
  [normalized-points iter]
  (let [mapped-by-x-val (group-by (fn [pos] (second pos)) normalized-points)
        count-per-row (map #(count (get mapped-by-x-val %)) (keys mapped-by-x-val))
        max-per-row (apply max count-per-row)
        more-lines (count (filter #(> % 20) count-per-row))
        _ (println "index: " iter "max per row:" max-per-row "average row size: ")
        ]
    (> more-lines 6))
  )


(defn solve-part-2
  [filename]
  (let [lines (io/read-input "day14/input.txt")
        size {:width 101 :height 103}
        cleaned-lines (map #(clean-string %) lines)
        parsed-lines (map #(parse-to-spec %) cleaned-lines)
        ]
    (loop [index 6000]
      (let [all-new-values (pmap #(multiply-position % index) parsed-lines)
            all-normalized (pmap #(normalize-position % size) all-new-values)
            _ (when (= 0 (mod index 1000)) (println "On index:" index))]
        (if (is-easter-egg2? all-normalized index)
          index
          (recur (inc index))))
      ))
  )




(defn print-board-for-iteration
  []
  (let [lines (io/read-input "day14/input.txt")
        ;;size {:width 11 :height 7}
        size {:width 101 :height 103}
        iter 7037
        cleaned-lines (pmap #(clean-string %) lines)
        parsed-lines (pmap #(parse-to-spec %) cleaned-lines)
        all-new-values (pmap #(multiply-position % iter) parsed-lines)
        all-normalized (pmap #(normalize-position % size) all-new-values)
        new-board (b/new 101 103)
        filled-in-board (b/update-board-data new-board (vec (repeat (* 101 103) " ")))
        filled-in-board2 (reduce (fn [board pos]
                                   (b/set-pos (first pos) (second pos) board "*"))
                                 filled-in-board
                                 all-normalized)
        _ (b/print-board filled-in-board2)
        ]
    )
  )

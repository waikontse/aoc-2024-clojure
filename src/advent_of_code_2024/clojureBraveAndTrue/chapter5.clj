(ns advent-of-code-2024.clojureBraveAndTrue.chapter5
  (:require [clojure.string :as s])
  )

(def great-name "hello-my-name-is")

(let [great-name "batman"]
  great-name)

great-name


(defn my-sum
  "docstring"
  ([vals] (my-sum vals 0))
  ([vals acc]
   (if (empty? vals)
     acc
     (recur (rest vals) (+ (first vals) acc)))))

(s/replace (s/trim " hheelllooo world lol  ") #"lol" "LOL")

(my-sum [1 2 3 4 5])

;;
;; Try memoizing results
;;
(defn sleepy-identity [x]
  (Thread/sleep 1000)
  x)

(def mem-sleepy-identity (memoize sleepy-identity))

(time (mem-sleepy-identity "batman"))
(time (mem-sleepy-identity "batman"))
(time (mem-sleepy-identity "batman"))

(def my-map {1 {:name "wkt" :age 39},
             2 {:name "robin" :age 25},
             3 {:name "batman" :age 30},
             })

(get my-map 2)
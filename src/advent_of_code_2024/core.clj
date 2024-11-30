(ns advent-of-code-2024.core
  (:require  [advent-of-code-2024.utils.io :as io]))


(defn -main
  "main entry to application"
  []
  (println  (System/getProperty "user.dir"))
  (let [lines (io/read-input "hello-world.txt")]
    (reduce println lines))
  (println "hello world"))

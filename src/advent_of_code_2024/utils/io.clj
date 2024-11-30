(ns advent-of-code-2024.utils.io)

(defn append-path
  "append the needed path to correctly read the filename"
  [filename]
  (str "./resources/" filename))

(defn read-input
  "Try to read the file and return the list of lines"
  [filename]
  (with-open [rdr (clojure.java.io/reader (append-path filename))]
    (into [] (line-seq rdr))))


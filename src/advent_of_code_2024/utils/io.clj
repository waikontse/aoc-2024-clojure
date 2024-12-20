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

(defn str->int
  "Convert a string to integer"
  [str]
  (Long/parseLong str))

(defn sum
  "Sum all the values into the coll"
  [coll]
  (reduce + coll))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))
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

(defn in?
  ""
  [value coll]
  (boolean (some #(= value %) coll)))

(defn char->str
  "convert a Character to string"
  [char]
  (str char))

(defn chars->strs
  "Converts a collection of characters to strings"
  [coll]
  (map #(char->str %) coll))

(defn str->int
  "Convert a string to integer"
  [str]
  (Long/parseLong str))

(defn strs->ints
  "Convert a collection of strings to longs"
  [coll]
  (map #(str->int %) coll))

(defn sum
  "Sum all the values into the coll"
  [coll]
  (reduce + coll))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn swap-vec
  "swap the values of a vector between 2 indexes"
  [coll idx1 idx2]
  (let [val-1 (get coll idx1)
        val-2 (get coll idx2)]
    (-> coll
        (assoc idx1 val-2)
        (assoc idx2 val-1))))

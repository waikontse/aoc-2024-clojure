(ns advent-of-code-2024.snippets.playground)

(defn two-comp
  "docstring"
  [f g]
  (fn [& args]
    (f (apply g args))))

(comp + *)
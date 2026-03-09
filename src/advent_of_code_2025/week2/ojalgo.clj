(ns advent-of-code-2025.week2.ojalgo
  (:import [org.ojalgo.optimisation ExpressionsBasedModel Variable])
  )

;"a >= 0",
;"b >= 0",
;"c >= 0",
;"d >= 0",
;"e >= 0",
;
;"a + c + d = 7",
;"d + e = 5",
;"a + b + d + e = 12",
;"a + b + e = 7",
;"a + c + e = 2",
(defn create-integer-var
  ^Variable
  [model name]
  (doto
    (.addVariable model name)
    (.setInteger true)))

(defn solve-model []
  (let [model (ExpressionsBasedModel.)
        a (create-integer-var model "a")
        b (create-integer-var model "b")
        c (create-integer-var model "c")
        d (create-integer-var model "d")
        e (create-integer-var model "e")

        constraint1 (doto
                      (.addExpression model "constraint-1")
                      (.set a 1)
                      (.set c 1)
                      (.set d 1)
                      (.level 7)
                      )
        ]
    )
  )
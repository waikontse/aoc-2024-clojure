(ns advent-of-code-2024.snippets.ojalgo
  (:import
    [org.ojalgo.optimisation ExpressionsBasedModel Variable])
  )

(defn solve-a []
  ;#{
  ;  "a >= 0",
  ;  "b >= 0",
  ;  "c >= 0",
  ;  "d >= 0",
  ;  "e >= 0",
  ;  "f >= 0",
  ;

  ;  }

  (let [model (ExpressionsBasedModel.)
        a (doto (.addVariable model "a")
            (.lower 0)
            (.integer true))
        b (doto (.addVariable model "b")
            (.lower 0)
            (.integer true))
        c (doto (.addVariable model "c")
            (.lower 0)
            (.integer true))
        d (doto (.addVariable model "d")
            (.lower 0)
            (.integer true))
        e (doto (.addVariable model "e")
            (.lower 0)
            (.integer true))
        f (doto (.addVariable model "f")
            (.lower 0)
            (.integer true))

        ;; Add constraints
        ;; Add constraint: 2x1 + 3x2 + 4x3 <= 5
        ;  "e + f = 3",
        ;  "b + f = 5",
        ;  "c + d + e = 4",
        ;  "a + b + d = 7",
        weight-constraint (doto (.addExpression model "weight")
                            (.level 3)
                            (.set e 1)
                            (.set f 1)
                            )
        weight-constraint2 (doto (.addExpression model "weight2")
                            (.level 5)
                            (.set b 1)
                            (.set f 1)
                            )

        weight-constraint3 (doto (.addExpression model "weight3")
                            (.level 4)
                            (.set c 1)
                            (.set d 1)
                            (.set e 1)
                            )
        weight-constraint4 (doto (.addExpression model "weight4")
                             (.level 7)
                             (.set a 1)
                             (.set b 1)
                             (.set d 1)
                             )
        result (.minimise model)]
    (println "Status:" (.toString result))
    (println "Objective value:" (.getValue result))
    (println "x1 =" (.getValue a))
    (println "x2 =" (.getValue b))
    (println "x3 =" (.getValue c))
    (println "x4 =" (.getValue d))
    (println "x5 =" (.getValue e))
    (println "x6 =" (.getValue f))))



(defn solve-knapsack []
  (let [model (ExpressionsBasedModel.)

        ;; Define binary decision variables (0 or 1)
        x1 (doto (.addVariable model "x1")
             (.weight 5)          ; objective coefficient (maximize value)
             (.lower 0)
             (.upper 1)
             (.integer true))     ; <-- makes it an integer variable

        x2 (doto (.addVariable model "x2")
             (.weight 4)
             (.lower 0)
             (.upper 1)
             (.integer true))

        x3 (doto (.addVariable model "x3")
             (.weight 3)
             (.lower 0)
             (.upper 1)
             (.integer true))

        ;; Add constraint: 2x1 + 3x2 + 4x3 <= 5
        weight-constraint (doto (.addExpression model "weight")
                            (.upper 5)
                            (.set x1 2)
                            (.set x2 3)
                            (.set x3 4))

        ;; Solve (maximise)
        result (.maximise model)]

    (println "Status:" (.toString result))
    (println "Objective value:" (.getValue result))
    (println "x1 =" (.getValue x1))
    (println "x2 =" (.getValue x2))
    (println "x3 =" (.getValue x3))))

(defn -main [& _args]
  (solve-a))



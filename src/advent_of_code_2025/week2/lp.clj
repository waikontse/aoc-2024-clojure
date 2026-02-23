(ns advent-of-code-2025.week2.lp)

(require '[prolin :as p])
(require '[prolin.protocols :as pp])
(require '[prolin.commons-math :as cm])

;; Maximize x
(p/optimize (cm/solver) "x" #{"x <= 5", "x >= -2"} false)
;; => {"x" 5.0}

;; Same as above
(p/maximize (cm/solver) "x" #{"x <= 5", "x >= -2"})
;; => {"x" 5.0}

;; Now minimizing
(p/minimize (cm/solver) "x1" #{"x <= 5", "x >= -2"})
;; => {"x" -2.0}

(p/minimize (cm/solver) "x1"
            #{"x <= 5",
              "x >= -2",
              "x = 3"}
            )

(p/minimize (cm/solver) "a"
            #{
              "a >= 0",
              "b >= 0",
              "c >= 0",
              "d >= 0",
              "e >= 0",
              "f >= 0",

              "e + f = 3",
              "b + f = 5",
              "c + d + e = 4",
              "a + b + d = 7",
              }
            )

(def ans-b (p/minimize (cm/solver) "e"
                       #{
                         "a >= 0",
                         "b >= 0",
                         "c >= 0",
                         "d >= 0",
                         "e >= 0",

                         "a + c + d = 7",
                         "d + e = 5",
                         "a + b + d + e = 12",
                         "a + b + e = 7",
                         "a + c + e = 2",
                         }
                       ))

(int (reduce +(vals ans-b)))

;; Using more than one variable
(p/maximize (cm/solver) "x" #{"2x = y", "y <= 5"})
;; => {"x" 5.0, "y" 2.5}

;; Same as above, but constructing objective & constraints directly,
;; instead of using the String implementations
(p/maximize (cm/solver)
            (pp/linear-polynomial 0 {:x 1})
            #{(pp/constraint '= (pp/linear-polynomial 0 {:x 2 :y -1}))
              (pp/constraint '<= (pp/linear-polynomial -5 {:y 1}))})
;; => {:x 5.0, :y 2.5}

;; Throws an ex-info with a :reason of :no-solution if it can't be solved
(p/maximize (cm/solver) "x" #{"x = 3", "x = 4"})
;; => Exception!

;; Throws an ex-info with a :reason of :unbounded if the solution
;; is unconstrainted
(p/maximize (cm/solver) "x" #{"x >= 1"})
;; => Exception!


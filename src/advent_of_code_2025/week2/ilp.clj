(ns advent-of-code-2025.week2.ilp
  (:import [org.ojalgo.optimisation ExpressionsBasedModel]))

;; ==============================================================================
;; Helper Functions
;; ==============================================================================

(defn create-model
  "Create a new ExpressionsBasedModel for optimization"
  []
  (ExpressionsBasedModel.))

(defn add-variable
  "Add a variable to the model with optional bounds and integer constraint
   Options:
   - :lower - lower bound (default 0)
   - :upper - upper bound (default nil = unbounded)
   - :integer? - whether variable must be integer (default true for ILP)
   - :name - variable name (default 'x0', 'x1', etc.)"
  ([model name]
   (add-variable model name {}))
  ([model name {:keys [lower upper integer?]
                :or   {lower 0 integer? true}}]
   (let [var (.addVariable model (str name))]
     (.lower var (double lower))
     (when upper
       (.upper var (double upper)))
     (when integer?
       (.integer var true))
     var)))

(defn set-objective
  "Set the objective function for the model
   direction: :minimize or :maximize
   coefficients: map of {variable -> coefficient}"
  [model direction coefficients]
  (let [expr (.addExpression model "objective")]
    (doseq [[var coef] coefficients]
      (.set expr var (double coef)))
    (case direction
      :minimize (.weight expr 1)
      :maximize (.weight expr -1))
    expr))

(defn add-constraint
  "Add a constraint to the model
   name: constraint name
   coefficients: map of {variable -> coefficient}
   relation: :>= :== :<= (greater-than-or-equal, equal, less-than-or-equal)
   bound: the right-hand side value"
  [model name coefficients relation bound]
  (let [expr (.addExpression model (str name))]
    (doseq [[var coef] coefficients]
      (.set expr var (double coef)))
    (case relation
      :>= (.lower expr (double bound))
      :== (.level expr (double bound))
      :<= (.upper expr (double bound)))
    expr))

(defn solve
  "Solve the optimization model and return results"
  [model]
  (let [result (.minimise model)]
    {:status          (.getState result)
     :objective-value (.getValue result)
     :solution        (into {}
                            (map (fn [var]
                                   [(.getName var)
                                    (.getValue result var)])
                                 (.getVariables model)))}))

;; ==============================================================================
;; Example 1: Simple Knapsack Problem
;; ==============================================================================

(defn knapsack-example
  "Solve a simple knapsack problem
   Maximize: 4x1 + 3x2 + 6x3 + 5x4
   Subject to:
     2x1 + 1x2 + 3x3 + 2x4 <= 10 (weight constraint)
     x1, x2, x3, x4 ∈ {0,1} (binary variables)"
  []
  (let [model (create-model)
        ;; Create binary variables (0 or 1)
        x1 (add-variable model "x1" {:lower 0 :upper 1 :integer? true})
        x2 (add-variable model "x2" {:lower 0 :upper 1 :integer? true})
        x3 (add-variable model "x3" {:lower 0 :upper 1 :integer? true})
        x4 (add-variable model "x4" {:lower 0 :upper 1 :integer? true})]

    ;; Maximize objective (note: we negate for minimization solver)
    (set-objective model :maximize {x1 4, x2 3, x3 6, x4 5})

    ;; Add weight constraint
    (add-constraint model "weight" {x1 2, x2 1, x3 3, x4 2} :<= 10)

    ;; Solve and return result
    (solve model)))

;; ==============================================================================
;; Example 2: Production Planning
;; ==============================================================================

(defn production-planning-example
  "Solve a production planning problem
   A company produces two products A and B.
   Product A: profit = $40, requires 2 hours machine time, 1 hour labor
   Product B: profit = $30, requires 1 hour machine time, 2 hours labor
   Available: 100 hours machine time, 80 hours labor
   Find optimal production quantities (must be integers)"
  []
  (let [model (create-model)
        ;; Number of products A and B to produce
        a (add-variable model "product_A" {:lower 0 :integer? true})
        b (add-variable model "product_B" {:lower 0 :integer? true})]

    ;; Maximize profit
    (set-objective model :maximize {a 40, b 30})

    ;; Machine time constraint
    (add-constraint model "machine_time" {a 2, b 1} :<= 100)

    ;; Labor time constraint
    (add-constraint model "labor_time" {a 1, b 2} :<= 80)

    (solve model)))

;; ==============================================================================
;; Example 3: Assignment Problem
;; ==============================================================================

(defn assignment-problem-example
  "Solve a simple assignment problem
   Assign 3 workers to 3 tasks to minimize total cost
   Cost matrix (worker x task):
     [[9 2 7]
      [6 4 3]
      [5 8 1]]
   Each worker does exactly one task, each task done by exactly one worker"
  []
  (let [model (create-model)
        costs [[9 2 7]
               [6 4 3]
               [5 8 1]]
        n 3
        ;; Create binary decision variables x_ij (worker i does task j)
        vars (vec (for [i (range n)]
                    (vec (for [j (range n)]
                           (add-variable model
                                         (str "x_" i "_" j)
                                         {:lower 0 :upper 1 :integer? true})))))]

    ;; Minimize total cost
    (set-objective model :minimize
                   (into {}
                         (for [i (range n)
                               j (range n)]
                           [(get-in vars [i j]) (get-in costs [i j])])))

    ;; Each worker does exactly one task
    (doseq [i (range n)]
      (add-constraint model
                      (str "worker_" i)
                      (zipmap (get vars i) (repeat 1))
                      :== 1))

    ;; Each task done by exactly one worker
    (doseq [j (range n)]
      (add-constraint model
                      (str "task_" j)
                      (zipmap (map #(get-in vars [% j]) (range n)) (repeat 1))
                      :== 1))

    (solve model)))

;; ==============================================================================
;; Example 4: Advent of Code Day 13 Style Problem (Prize Machine)
;; ==============================================================================

(defn solve-prize-machine
  "Solve a prize machine problem like AoC 2024 Day 13
   Button A: costs 3 tokens, moves X by ax and Y by ay
   Button B: costs 1 token, moves X by bx and Y by by
   Find minimum tokens to reach target (tx, ty)"
  [ax ay bx by tx ty]
  (let [model (create-model)
        ;; Number of times to press each button
        a (add-variable model "button_a" {:lower 0 :integer? true})
        b (add-variable model "button_b" {:lower 0 :integer? true})]

    ;; Minimize cost (3 tokens for A, 1 token for B)
    (set-objective model :minimize {a 3, b 1})

    ;; X position constraint
    (add-constraint model "x_position" {a ax, b bx} :== tx)

    ;; Y position constraint
    (add-constraint model "y_position" {a ay, b by} :== ty)

    (solve model)))

;; ==============================================================================
;; Running Examples
;; ==============================================================================

(defn run-all-examples
  "Run all ILP examples and display results"
  []
  (println "=== Example 1: Knapsack Problem ===")
  (let [result (knapsack-example)]
    (println "Status:" (:status result))
    (println "Maximum value:" (- (:objective-value result))) ; negated because we use minimize
    (println "Solution:" (:solution result))
    (println))

  (println "=== Example 2: Production Planning ===")
  (let [result (production-planning-example)]
    (println "Status:" (:status result))
    (println "Maximum profit: $" (- (:objective-value result)))
    (println "Solution:" (:solution result))
    (println))

  (println "=== Example 3: Assignment Problem ===")
  (let [result (assignment-problem-example)]
    (println "Status:" (:status result))
    (println "Minimum cost:" (:objective-value result))
    (println "Solution:" (:solution result))
    (println))

  (println "=== Example 4: Prize Machine ===")
  (let [result (solve-prize-machine 94 34 22 67 8400 5400)]
    (println "Status:" (:status result))
    (println "Minimum tokens:" (:objective-value result))
    (println "Solution:" (:solution result))
    (println)))

(comment
  ;; Run individual examples
  (knapsack-example)
  (production-planning-example)
  (assignment-problem-example)
  (solve-prize-machine 94 34 22 67 8400 5400)

  ;; Run all examples
  (run-all-examples)
  )




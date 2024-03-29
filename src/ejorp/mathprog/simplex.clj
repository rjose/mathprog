;; ## Simplex Algorithm
;;
;; This implements the simplex algorithm for solving linear programs as
;; laid out by Bradley, Hax, and Magnanti.

(ns ejorp.mathprog.simplex
  (:require (ejorp.mathprog [util :as util]
                            [tableau :as tableau])))

;; ## Rows, Constraints, and Objectives, and Tableaus
;; 
;; When we talk about a "row", we mean something that has the following form
;;
;;       {:val 0, :coeffs [6 14 13 0 0]}
;; 
;; The `:val` field is the RHS of the equation. The `:coeffs` field contains the
;; coefficients of the decision variables.
;;
;; Objectives have the same form as rows. When a tableau is optimal,
;; `:val` is the optimal value of the problem.
;; 
;; Constraint rows also have a `basic-idx` field. This gives the
;; index of the basic variable associated with that constraint. This
;; means that the coefficient of the basic variable is 1 in the
;; constraint and zero in all other constraints and zero in the
;; objective. 
;; 
;; A tableau combines the objective and constraints for a problem. It has
;; the following form:
;;
;;       {:objective objective,
;;        :constraints [constraint1, ...],
;;        :status :optimal}
;;
;; The `:status` field indicates if the tableau has reached its final state.

;; ## Pivoting
;;
;; Pivoting happens when a new nonbasic variable enter the basis,
;; displacing one of the previous basic variables. 
;;
;; One way to determine which variable should enter the basis next is to look at
;; the objective function and pick the variable with the greatest coefficient.
;; The rationale is that this variable will have the greatest impact on the
;; objective.  This is how `next-basic-idx` works.
;;
;; To determine which basic variable should leave a set when pivoting a new
;; variable in, we look at the ratio of each constraints `:val` to the
;; coefficient of the candidate basic variable. The constraint with the minimum
;; ratio is the one that will become binding first. The `pivot-row-idx` function
;; computes this for us.



;; ### Pivoting Functions

(defn next-basic-idx
  "Returns the index of the next variable to enter the basis by looking at the
current `objective`."
  [{objective :objective :as tableau} ]
  (let [coeffs (:coeffs objective)
        var-idx (util/idx-max coeffs)
        coeff (nth coeffs var-idx)]
    (if (<= coeff 0.0) nil var-idx)))

(defn ratio
  "Returns the ratio of the `:val` of a constraint to the coefficient of the
specified decision variable. This is used to determine how quickly a constraint
will become binding as the decision variable is changed."
  [row var-idx]
  (let [coeff (-> (:coeffs row) (nth var-idx))]
    (if (or (nil? coeff) (= 0 coeff)) Double/NaN (/ (:val row) coeff))))


(defn pivot-row-idx
  "Returns the index of the constraint that will be used for the next simplex
pivot for the decision variable with index `var-idx`. In effect, this returns
the basic variable that will leave when the new basic variable comes in."
  [{constraints :constraints :as tableau}  var-idx]
  (let [ratios (map #(ratio % var-idx) constraints)
        pegged-ratios (map #(if (or (Double/isNaN %) (<= % 0)) Double/MAX_VALUE %) ratios)]
    (if (every? #(= % Double/MAX_VALUE) pegged-ratios) nil (util/idx-min pegged-ratios))))


;; ## Executing the Simplex Algorithm
;;
;; The collection of tableau pivots from initial tableau to the solution tableau
;; can be viewed as a lazily computed seq. Each of these tableaus is computed by
;; applying the `simplex-next` function to the previous tableau. At each pivot we
;; figure out the new state of the tableau.
;;
;; To solve an LP using the simplex algorithm, we simply drop all tableaus
;; from this seq for which `:status` is nil and then take the next tableau
;; as the solution. This is essentially what `solve` does.

;; ### Simplex Functions

(defn optimal?
  "Returns true if the tableau is optimal"
  [{objective :objective, constraints :constraints}]
  (let [b-idxs (tableau/basic-idxs constraints) 
        obj-coeffs (:coeffs objective)
        non-b-obj-coeffs (util/exclude-idxs obj-coeffs b-idxs)]
    (and (every? #(<= % 0) non-b-obj-coeffs)
           (every? #(>= (-> % :val) 0) constraints))))


(defn unbounded?
  "true if the problem is unbounded with respect to the next
basic variable; false otherwise"
  [tableau]
  (if-let [var-idx (next-basic-idx tableau)]
    (and (pos? (-> tableau :objective :coeffs (nth var-idx)))
             (every? #(<= (-> % :coeffs (nth var-idx))  0) (-> tableau :constraints)))
    false))

(defn tableau-status
  "Given a tableau in canonical form, returns its status; one of:
`:optimal`, `:unbounded`, or `nil`."
  [tableau]
  (cond
    (optimal? tableau) :optimal
    (unbounded? tableau) :unbounded
    :else nil))

(defn has-converged?
  "`true` if the tableau is `:optimal` or `:unbounded`; `false` otherwise."
  [status]
  (cond
    (= status :optimal) true
    (= status :unbounded) true
    :else false))

(defn pivot
  "Returns the updated tableau given a tableau and the index of the
  pivot constraint and the index of the variable entering the basis"
  [{objective :objective, constraints :constraints, status :status :as tableau}
   constraint-idx entering-idx]
  (let [tableau (tableau/add-constraint-map tableau)
        constraint (tableau/set-basic-var (nth constraints constraint-idx) entering-idx)
        new-objective (tableau/eliminate-basic-var objective constraint)
        new-constraints (util/insert-at
                         (map #(tableau/eliminate-basic-var % constraint)
                              (util/exclude-nth constraints constraint-idx))
                         constraint-idx constraint)
        new-tableau {:objective new-objective, :constraints new-constraints}]
    (merge new-tableau {:status (tableau-status new-tableau)})))


(defn simplex-next
  "Returns the next tableau in the simplex algorithm sequence given a tableau"
  [{objective :objective, constraints :constraints, status :status :as tableau}]
  (if (has-converged? status)
    tableau
    (let [next-basic-idx (next-basic-idx tableau)
          constraint-idx (pivot-row-idx tableau next-basic-idx)]
      (pivot tableau constraint-idx next-basic-idx))))


(defn tableau-seq
  "Returns a lazy, infinite seq of tableaus for each iteration of the simplex method"
  [initial-tableau]
  (iterate simplex-next initial-tableau))

(defn solve
  "Takes an LP in the form of a tableau and returns the solution in the form of a tableau."
  [initial-tableau]
  (first (drop-while #(nil? (:status %)) (tableau-seq initial-tableau))))

(defn is-primal-canonical?
  "`true` if the tableau is in primal canonical form."
  [tableau]
  (every? #(and (>= (:val %) 0)
                (-> % :basic-idx nil? not)) (-> tableau :constraints)))



; TODO: Add checks for canonical before attempting to solve

;; ## Fixtures
(def row1 {:val 24, :basic-idx 3, :coeffs [0.5 2 1 1 0]})
(def row2 {:val 60, :basic-idx 4, :coeffs [1 2 4 0 0]})
(def objective {:val 0, :coeffs [6 14 13 0 0]})
(def constraints [row1 row2])
(def problem {:objective objective, :constraints constraints})




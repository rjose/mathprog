;; This implements the simplex algorithm for solving linear programs as
;; laid out by Bradley, Hax, and Magnanti.

; TODO: Rewrite functions to take a tableau instead of specific constraints or objectives

(ns ejorp.mathprog.simplex
  (:require (ejorp.mathprog [util :as util]
                            [tableau :as tableau])))

;; ## Rows, Constraints, and Objectives, and Tableaus
;;
;; When we talk about a "row", we mean a map that has the following form
;;
;;       {:val 0, :coeffs [6 14 13 0 0]}
;;
;; The `:val` field is the RHS of the equation. The `:coeffs` field are the
;; coefficients of the decision variables.
;;
;; Objectives have the same form as rows. When a tableau is optimal,
;; `:val` is the optimal value of the problem.
;;
;; Constraints are rows that also have a `basic-idx` field. This gives the
;; index of the basic variable associated with that constraint.
;;
;; A tableau is a map that combines the objective and constraints for a problem. It has
;; the following form:
;;
;;       {:objective objective,
;;        :constraints [constraint1, ...],
;;        :should-stop false}
;;
;; The `should-stop` field indicates if the tableau has reached its final state.

;; ## Pivoting
;;
;; Pivoting happens when a new variable enter the basis, displacing one of the
;; previous basic variables. When the tableau is in canonical form, each
;; constraint is associated with a basic variable. This means that it appears in
;; that constraint with a coefficient of 1 and does not appear in any other
;; constraint (or the objective function).
;;
;; One way to determine which variable should enter the basis next is to look at
;; the objective function and pick the variable with the greatest coefficient.
;; The rationale is that this variable will have the greatest impact on the
;; objective.  This is how `next-basic-var-idx` works.
;;
;; To determine which basic variable should leave a set when pivoting a new
;; variable in, we look at the ratio of each constraints `:val` to the
;; coefficient of the candidate basic variable. The constraint with the minimum
;; ratio is the one that will become binding first. The `pivot-row-idx` function
;; computes this for us.

;; ### Pivoting Support Functions

(defn next-basic-var-idx
  "Returns the index of the next variable to enter the basis by looking at the
current `objective`."
  [objective]
  (let [coeffs (:coeffs objective)
        var-idx (util/idx-max coeffs)
        coeff (nth coeffs var-idx)]
    (if (<= coeff 0.0)
      nil
      var-idx)))

(defn ratio
  "Returns the ratio of the `:val` of a constraint to the coefficient of the
specified decision variable. This is used to determine how quickly a constraint
will become binding as the decision variable is changed.
"
  [row var-idx]
  (let [coeff (-> (:coeffs row) (nth var-idx))]
    (if (or (nil? coeff) (= 0 coeff))
      Double/NaN
      (/ (:val row) coeff))))


(defn peg-values-if-necessary
  "This replaces a value in a vector with Dobule/MAX_VALUE if the
value is NaN or non-positive."
  [v]
  (map #(cond
          (Double/isNaN %) Double/MAX_VALUE
          (<= % 0.0) Double/MAX_VALUE
          :else %) v))

(defn pivot-row-idx
  "Returns the index of the constraint that will be used for the next simplex
pivot for the decision variable with index `var-idx`. In effect, this returns
the basic variable that will leave when the new basic variable comes in.

If the solution is unbounded, `nil` is returned."
  [rows var-idx]
  (let [ratios (map #(ratio % var-idx) rows)
        pegged-ratios (peg-values-if-necessary ratios)]
    (if (every? #(= % Double/MAX_VALUE) pegged-ratios)
      nil
      (util/idx-min pegged-ratios))))


;; ## Executing the Simplex Algorithm
;;
;; The collection of tableau pivots from initial tableau to the solution tableau
;; can be viewed as a lazily computed seq. Each of these tableaus is computed by
;; applying the `pivot` function to the previous tableau. At each pivot we
;; figure out if there another pivot is required. This is stored as
;; `:should-stop` in each tableau.
;;
;; To solve an LP using the simplex algorithm, we simply drop all tableaus
;; from this seq for which `:should-stop` is false and then take the next tableau
;; as the solution. This is essentially what `solve` does.

;; ### Simplex Functions

(defn optimal?
  "Returns true if the tableau is optimal"
  [{objective :objective, constraints :constraints}]
  (let [b-idxs (tableau/basic-idxs constraints) 
        obj-coeffs (:coeffs objective)
        non-b-obj-coeffs (util/exclude-idxs obj-coeffs b-idxs)]
    (cond
      (every? #(<= % 0) non-b-obj-coeffs) true
      :else false)))

(defn tableau-status
  [is-optimal pivot-row-idx]
  (cond
    is-optimal :optimal
    (nil? pivot-row-idx) :unbounded
    :else nil))

(defn should-stop
  "`true` if the tableau is `:optimal` or `:unbounded`; `false` otherwise."
  [status]
  (cond
    (= status :optimal) true
    (= status :unbounded) true
    :else false))

(defn pivot
  "Takes a tableau in canonical form and returns the next tableau in the simplex
sequence. If the tableau no longer requires a pivot, the input tableau is returned."
  [{objective :objective, constraints :constraints, status :status :as tableau}]
  (if (should-stop status) 
    tableau
    (let [next-basic-idx (next-basic-var-idx objective)
          constraint-idx (pivot-row-idx constraints next-basic-idx)
          constraint (tableau/set-basic-var (nth constraints constraint-idx) next-basic-idx)
          new-objective (tableau/eliminate-basic-var objective constraint)
          new-constraints (util/insert-at
                           (map #(tableau/eliminate-basic-var % constraint)
                                (util/exclude-nth constraints constraint-idx))
                           constraint-idx constraint)
          is-optimal (optimal? {:objective new-objective, :constraints new-constraints})]
      {:objective new-objective, :constraints new-constraints,
       :status (tableau-status is-optimal constraint-idx)})))


(defn tableau-seq
  "Returns a lazy, infinite seq of tableaus for each iteration of the simplex method"
  [initial-tableau]
  (iterate pivot initial-tableau))

(defn solve
  "Takes an LP in the form of a tableau and returns the solution in the form of a tableau."
  [initial-tableau]
  (first (drop-while #(nil? (:status %)) (tableau-seq initial-tableau))))


; Dual simplex functions
(defn dual-pivot-row-idx
  "Returns index of constraint to be used as the pivot in the
  dual-simplex method"
  [tableau]
  (util/idx-min (:constraints tableau) :val))

; TODO: Test if there are no negative coeffs in the constraint
(defn dual-next-basic-idx
  "Returns the index of the nonbasic variable that should enter the
  basis next given a pivot row idx"
  [tableau row-idx]
  (let [constraint-coeffs (-> tableau :constraints (nth row-idx) :coeffs)
        neg-coeff? (map neg? constraint-coeffs)
        neg-non-basic-idxs (filter #(nth neg-coeff? %) (tableau/non-basic-idxs (:constraints tableau)))
        obj-coeffs (-> tableau :objective :coeffs)]
    (nth neg-non-basic-idxs
         (util/idx-min (for [i neg-non-basic-idxs] (/ (nth obj-coeffs i) (nth constraint-coeffs i)))))))

; TODO: Rewrite pivot function so it takes a nonbasic index and a basic idx
; TODO: Rewrite other functions to return indexes that can be used in the pivot


;; ## Fixtures
(def row1 {:val 24, :basic-idx 3, :coeffs [0.5 2 1 1 0]})
(def row2 {:val 60, :basic-idx 4, :coeffs [1 2 4 0 0]})
(def objective {:val 0, :coeffs [6 14 13 0 0]})
(def constraints [row1 row2])
(def problem {:objective objective, :constraints constraints})

(def dual-tableau1
  {
   :objective {:val 0, :coeffs [-3 -1 0 0]},
   :constraints [{:val -1, :basic-idx 2, :coeffs [-1 -1 1 0]}
                 {:val -2, :basic-idx 3, :coeffs [-2 -3 0 1]}]})
(ns com.ejorp.mathprog.core
  (:require (com.ejorp.mathprog [util :as util]
                                [tableau :as tableau])))


(defn ratio
  "Returns the ratio used to determine which constraint row to pivot from"
  [row var-idx]
  (let [coeff (-> (:coeffs row) (nth var-idx))]
    (if (or (nil? coeff) (= 0 coeff))
      (throw (RuntimeException. "Can't call ratio for variable without a coefficient"))
      (/ (:val row) coeff))))

; TODO: Ensure we handle the case where the coefficient is negative
(defn pivot-row-idx
  "Returns the index of the row that will be used for the next simplex pivot. `var-idx`
is the index of the decision variable that we'd like to enter the basis."
  [rows var-idx]
  (let [ratios (map #(ratio % var-idx) rows)]
    (util/idx-min ratios)))

(defn next-basic-var-idx
  "Returns the index of the next basic variable to enter the basis."
  [objective]
  ;; TODO: Figure out who should handle the case where there is no next basic variable
  (util/idx-max (:coeffs objective)))

;; TODO: Handle unbounded case
(defn should-pivot
  "Returns false if the pivot result is optimal, infeasible, or unbounded"
  [{objective :objective, constraints :constraints}]
  (let [b-idxs (tableau/basic-idxs constraints) 
        obj-coeffs (:coeffs objective)
        non-b-obj-coeffs (util/exclude-idxs obj-coeffs b-idxs)]
    (cond
      (every? #(<= % 0) non-b-obj-coeffs) false
      :else true)))


(defn pivot
  "Determines which variable should enter the basis and which one should leave given
`constraints` and an `objective`. Returns the updated constraints and objective"
  [{objective :objective, constraints :constraints, should-stop :should-stop :as tableau}]
  (if should-stop
    tableau
    (let [next-basic-idx (next-basic-var-idx objective)
          constraint-idx (pivot-row-idx constraints next-basic-idx)
          constraint (tableau/set-basic-var (nth constraints constraint-idx) next-basic-idx)
          new-objective (tableau/eliminate-basic-var objective constraint)
          new-constraints (util/insert-at
                           (map #(tableau/eliminate-basic-var % constraint) (util/exclude-nth constraints constraint-idx))
                           constraint-idx constraint)
          new-should-stop (not (should-pivot {:objective new-objective, :constraints new-constraints}))
          ]
      {:objective new-objective, :constraints new-constraints, :should-stop new-should-stop})))



(defn tableau-seq
  "Returns a lazy, infinite seq of tableaus for each iteration of the simplex method"
  [initial-tableau]
  (iterate pivot initial-tableau))

(defn solve
  "Takes an LP in the form of a tableau and returns the solution in the form of a tableau."
  [initial-tableau]
  (first (drop-while #(not (:should-stop %)) (tableau-seq initial-tableau))))



;; Fixtures
(def row1 {:val 24, :basic-idx 3, :coeffs [0.5 2 1 1 0]})
(def row2 {:val 60, :basic-idx 4, :coeffs [1 2 4 0 0]})
(def objective {:val 0, :coeffs [6 14 13 0 0]})
(def constraints [row1 row2])
(def problem {:objective objective, :constraints constraints})
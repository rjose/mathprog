;; ## Dual Simplex Algorithm
;; 
;; The dual simplex algorithm can be used to solve an LP whose tableau
;; is in "dual canonical" form where the primal optimality conditions
;; are satisfied (all objective coefficients are nonpositive) but
;; where the constraint RHS may not all nonnegative. This is what
;; `is-dual-canonical?` does.
;;
;; This dual canonical form is seen when starting from a primal
;; optimal tableau and doing "what if" scenarios with the RHS of the
;; constraints. 

(ns ejorp.mathprog.dual-simplex
  (:require (ejorp.mathprog [util :as util]
                            [tableau :as tableau]
                            [simplex :as simplex])))

(defn is-dual-canonical?
  [tableau]
  (and (every? #(<= % 0) (-> tableau :objective :coeffs))))

(defn dual-pivot-row-idx
  "Returns index of constraint to be used as the pivot in the
  dual-simplex method. The heuristic is to use the most negative
  RHS."
  [tableau]
  (let [var-idx (util/idx-min (:constraints tableau) :val)]
    (if (neg? (-> tableau :constraints (nth var-idx) :val)) var-idx nil)))


(defn dual-next-basic-idx
  "Returns the index of the nonbasic variable that should enter the
  basis next given a pivot row idx. We do this by finding the
  objective coefficient that will reach zero first."
  [tableau row-idx]
  (let [constraint-coeffs (-> tableau :constraints (nth row-idx) :coeffs)
        neg-coeff? (map neg? constraint-coeffs)
        neg-non-basic-idxs (filter #(nth neg-coeff? %) (tableau/non-basic-idxs (:constraints tableau)))
        obj-coeffs (-> tableau :objective :coeffs)]
    (nth neg-non-basic-idxs
         (util/idx-min (for [i neg-non-basic-idxs] (/ (nth obj-coeffs i) (nth constraint-coeffs i)))))))

(defn dual-simplex-next
  "Returns the next tableau in the dual-simplex algorithm."
  [{objective :objective, constraints :constraints, status :status :as tableau}]
  (if (simplex/has-converged? status)
    tableau
    (let [constraint-idx (dual-pivot-row-idx tableau)
          next-basic-idx (dual-next-basic-idx tableau constraint-idx)]
      (simplex/pivot tableau constraint-idx next-basic-idx))))

(defn dual-tableau-seq
  "Returns a lazy, infinite seq of tableaus for each iteration of the simplex method"
  [initial-tableau]
  (iterate dual-simplex-next initial-tableau))

(defn dual-solve
  "Takes an LP in the form of a tableau and returns the solution in the form of a tableau."
  [initial-tableau]
  (first (drop-while #(nil? (:status %)) (dual-tableau-seq initial-tableau))))


(def dual-tableau1
  {
   :objective {:val 0, :coeffs [-3 -1 0 0]},
   :constraints [{:val -1, :basic-idx 2, :coeffs [-1 -1 1 0]}
                 {:val -2, :basic-idx 3, :coeffs [-2 -3 0 1]}]})

;; This implements functions for doing sensitivity analysis on
;; solutions to linear programs.

(ns ejorp.mathprog.sensitivity
  (:require (ejorp.mathprog [tableau :as tableau]
                            [util :as util])))

;; ## Shadow prices and reduced cost
;;
;; The *shadow price* of a constraint is a measure of how much the
;; objective would increase if you were to increase the RHS of that
;; constraint. Equivalently, this is how much you would be willing to
;; pay to acquire more of this resource, hence its interpretation as a
;; price. The final tableau has this information in the objective
;; coefficients.
;;
;; The *reduced cost* is essentially the shadow cost of a decision
;; variable. In effect, this gives the amount the objective would
;; **decrease** if you required a decision variable to be at a certain
;; non-zero level.

;; ### Implementation

(defn- get-prices
  "Returns the shadow prices for the specified variables."
  [end-tableau idxs]
  (if (not (= :optimal (:status end-tableau)))
    nil
    (let [prices (:coeffs (:objective end-tableau))]
      (map #(- (nth prices %)) idxs))))

(defn shadow-prices
  "Returns a vector of shadow prices for each resource constraint."
  [start-tableau end-tableau]
  (let [slack-vars (tableau/basic-idxs (:constraints start-tableau))]
    (get-prices end-tableau slack-vars)))

(defn reduced-costs
  "Returns the reduced cost of for each decision variable."
  [start-tableau end-tableau]
  (let [decision-vars (tableau/non-basic-idxs (:constraints start-tableau))]
    (get-prices end-tableau decision-vars)))


;; ## Sensitivity of Objective Coeffs
;; We can use information from the initial and final tableaus to
;; determine how sensitive the optimal solution is with respect to
;; changes in the objective function.
;; 
;; The sensitivity computation for nonbasic variables in the final
;; tableau is simpler than the general version for basic variables, so
;; we have this implemented as a special case.
;; 
;; For a coefficient `c`, we use `[:lt 10]` to mean
;; `c <= 10`. Likewise `[:gt 4]` means `c >= 4`.
;;
;; For a nonbasic variable in the final tableau, the only thing that
;; determines whether it should enter the basis is if, in the "final"
;; tableau, it has a positive coefficient. We can figure out when this
;; happens by looking at the difference between the initial
;; coefficient and the final one. That's what `non-basic-coeff-limits`
;; does.
;;
;; For a basic variable in the final tableau, we can figure out how
;; varying its initial objective coefficient impacts when it leaves
;; the basis and a new nonbasic variable enters.  We do this by
;; looking at the constraint associated with the basic variable and
;; seeing how each of the nonbasic coefficients in that constraint
;; change with the initial objective coefficient. The point at which
;; these coefficients become positive is when they enter the basis.
;; That's what `basic-coeff-limit-for-non-basic-var` does.
;;
;; To figure out the actual sensitivity range for a basic variable, we
;; need to compute the coeff limit for each non-basic variable and
;; then find the most restrictive limits. That's what
;; `basic-coeff-limits` does.


;; ### Implementation for nonbasic decision vars

(defn- non-basic-coeff-limits
  "Returns the max limit of a non-basic coeff 
for which the optimal solution remains unchanged."
  [start-tableau end-tableau var-idx]
  (let [start-coeffs (:coeffs (:objective start-tableau))
        end-coeffs (:coeffs (:objective end-tableau))
        max-limit (- (nth start-coeffs var-idx) (nth end-coeffs var-idx))]
    {:lt max-limit}))


;; ### Implementation for basic decision vars

(defn- basic-coeff-limit-for-non-basic-var
  "Returns the limit on the original objective coefficient for
  the basic variable `var-idx` such that `nonbasic-idx` enters the
  basis from the final tableau."
  [start-tableau end-tableau var-idx nonbasic-idx]
  (let [end-tableau (tableau/add-constraint-map end-tableau)
        initial-coeff (-> start-tableau :objective :coeffs (nth var-idx))
        non-basic-obj-coeff (-> end-tableau :objective :coeffs (nth nonbasic-idx))
        non-basic-constraint-coeff (-> end-tableau :constraint-map (get var-idx) :coeffs (nth nonbasic-idx))
        limit (if (= 0 non-basic-constraint-coeff)
                Double/MAX_VALUE
                (+ initial-coeff (/ non-basic-obj-coeff non-basic-constraint-coeff)))]
    (if (> non-basic-constraint-coeff 0)
      [:gt limit]
      [:lt limit])))


(defn- basic-coeff-limits
  "Returns the minimum and maximum values the coeff for a basic var
can take before it leaves the basis."
  [start-tableau end-tableau var-idx]
  (let [initial-coeff (-> start-tableau :objective :coeffs (nth var-idx))
        constraint-map (-> end-tableau tableau/add-constraint-map :constraint-map)
        constraint-coeffs (:coeffs (constraint-map var-idx))
        objective-coeffs (:coeffs (:objective end-tableau))
        
        non-basic-idxs (tableau/non-basic-idxs (:constraints end-tableau))
        limits (for [i non-basic-idxs]
                 (basic-coeff-limit-for-non-basic-var start-tableau end-tableau var-idx i))]
    {:gt (util/max-gt limits), :lt (util/min-lt limits)}))


;; ### Bringing it all together

(defn obj-coeff-limits
  "Returns the limits on the initial objective coefficients such that
the optimal solution in `end-tableau` is unchanged."
  [start-tableau end-tableau]
  (let [constraints (:constraints start-tableau)
        decision-idxs (range (- (-> constraints first :coeffs count) (count constraints)))
        basic-idxs (apply hash-set (tableau/basic-idxs (:constraints end-tableau)))]
    (for [i decision-idxs]
      (if (contains? basic-idxs i)
        (basic-coeff-limits start-tableau end-tableau i)
        (non-basic-coeff-limits start-tableau end-tableau i)))))


;; ## Sensitivity for RHS of Constraints
;; There are two cases when dealing with RHS. The first is where the
;; constraint is not binding, in other words, when the slack variable
;; for this constraint is in the basis. To figure out the sensitivity
;; limit here, we simply subtract the initial RHS from the value of
;; the slack variable. In effect, this is the slack in our slack.
;;
;; The second case is where the constraint is binding. This happens
;; when there is no slack in the constraint, in some sense, when all
;; of the resources associated with this constraint have been
;; exhausted. To figure out the sensitivity range here, we can think
;; of adding a small amount to the RHS of the constraint in question
;; and then finding the point at which any of the final basic
;; variables becomes negative, signaling the exit of that variable
;; from the basis. In effect, we are adding more of this resource up
;; to the point where it cannot all be used.

;; ### Implementation for basic slack variables

(defn- basic-rhs-limits
  "Returns the RHS limits when the slack variable is in the basis."
  [start-tableau end-tableau var-idx]
  (let [start-tableau (tableau/add-constraint-map start-tableau)
        end-tableau (tableau/add-constraint-map end-tableau)
        min-limit (- (-> start-tableau :constraint-map (get var-idx) :val)
                     (-> end-tableau :constraint-map (get var-idx) :val))]
    {:gt min-limit}))


;; ### Implementation for nonbasic slack variables

(defn- rhs-limit-for-slack-in-constraint
  "Returns the RHS limit for the specified slack with respect to the
specified constraint."
  [constraint slack-var-idx]
  (let [val (:val constraint)
        coeff (-> constraint :coeffs (nth slack-var-idx))
        limit (/ (- val) coeff)]
    (if (> coeff 0)
      [:gt limit]
      [:lt limit])))

(defn- non-basic-rhs-limits
  "Returns the RHS limits when the slack variable is not in the basis"
  [start-tableau end-tableau var-idx]
  (let [constraints (:constraints end-tableau)
        limits (map #(rhs-limit-for-slack-in-constraint % var-idx) constraints)]
    {:gt (util/max-gt limits), :lt (util/min-lt limits)}))


;; ### Bringing it all together

(defn rhs-limits
  "Gives the sensitivity ranges for the RHS of each constraint."
  [start-tableau end-tableau]
  (let [slack-vars (tableau/basic-idxs (:constraints start-tableau))
        basic-idxs (apply hash-set (tableau/basic-idxs (:constraints end-tableau)))]
    (for [i slack-vars] (if (contains? basic-idxs i)
                          (basic-rhs-limits start-tableau end-tableau i)
                          (non-basic-rhs-limits start-tableau end-tableau i)))))


;; ## Fixtures
(def row1 {:val 60, :basic-idx 3, :coeffs [6 5 8 1 0 0]})
(def row2 {:val 150, :basic-idx 4, :coeffs [10 20 10 0 1 0]}) 
(def row3 {:val 8, :basic-idx 5, :coeffs [1 0 0 0 0 1]})
(def objective {:val 0, :coeffs [5.0 4.5 6.0 0 0 0]})
(def constraints [row1 row2 row3])
(def start-tableau1 {:objective objective, :constraints constraints})

(def end-tableau1
  {:objective {:val -51.42857142857143,
               :coeffs [0.0 0.0 -0.5714285714285713 -0.7857142857142858 -0.02857142857142857 0.0]},
   :constraints [{:basic-idx 0, :val 45/7, :coeffs [1N 0N 11/7 2/7 -1/14 0N]}
                 {:basic-idx 1, :val 30/7, :coeffs [0N 1N -2/7 -1/7 3/35 0N]}
                 {:basic-idx 5, :val 11/7, :coeffs [0N 0N -11/7 -2/7 1/14 1N]}],
   :status :optimal})
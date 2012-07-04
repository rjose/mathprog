(ns ejorp.mathprog.tableau)

(defn divide-row
  "Divides a row by a number `divisor`"
  [row divisor]
  (let [val (/ (:val row) divisor)
        coeffs (map #(/ % divisor) (:coeffs row))]
    (merge row {:val val, :coeffs coeffs})))

(defn multiply-row
  "Multiplies a row by a number `factor`"
  [row factor]
  (let [val (* (:val row) factor)
        coeffs (map #(* % factor) (:coeffs row))]
    (merge row {:val val, :coeffs coeffs})))

(defn subtract-row
  "Subtracts one row from another"
  [r1 r2]
  (let [val (- (:val r1) (:val r2))
        coeffs (map - (:coeffs r1) (:coeffs r2))]
    (merge r1 {:val val, :coeffs coeffs})))

(defn set-basic-var
  "Sets a new basic var for a constraint. This ensures the basic variable's coefficient is 1."
  [constraint var-idx]
  (let [coeff (nth (:coeffs constraint) var-idx)
        new-constraint (divide-row constraint coeff)]
    (merge new-constraint {:basic-idx var-idx})))

; TODO: Need to throw an exception if there isn't a basic var associated with the constraint
(defn eliminate-basic-var
  "This removes a basic var from `row` using the associated `basis-constraint`"
  [row basis-constraint]
  (->> (nth (:coeffs row) (:basic-idx basis-constraint))
      (multiply-row basis-constraint)
      (subtract-row row)))

(defn basic-idxs
  "Returns the indexes for the basic variables in a constraint set"
  [constraints]
  (map #(:basic-idx %) constraints))

(defn non-basic-idxs
  "Returns the indexes for non-basic variables in a constraint set"
  [constraints]
  (let [b-idxs (apply hash-set (basic-idxs constraints)) 
        all-idxs (range (count (:coeffs (first constraints))))]
    (remove b-idxs all-idxs)))

(defn add-constraint-map
  "Adds a map of basic variable idx to constraint"
  [tableau]
  (if (:constraint-map tableau)
    tableau
    (let [constraints (:constraints tableau)
        constraint-map (zipmap (map :basic-idx constraints) constraints)]
    (merge tableau {:constraint-map constraint-map}))))


(ns ejorp.mathprog.util)

(defn idx-min
  "Returns the index of the minimum element of `v`"
  [v]
  (first (apply min-key second (map-indexed vector v))))

(defn idx-max
  "Returns the index of the maximum element of `v`"
  [v]
  (first (apply max-key second (map-indexed vector v))))

(defn exclude-nth
  "Returns a vector of items with the nth one excluded"
  [v nth]
  (map second (remove #(= nth (first %)) (map-indexed vector v))))

(defn exclude-idxs
  "Returns a vector of items with the specified indexes excluded"
  [v idxs]
  (let [idxs-set (apply hash-set idxs)]
    (map second (remove #(idxs-set (first %)) (map-indexed vector v)))))

(defn insert-at
  "Inserts an element `e` at `idx` within a vector `v`"
  [s idx e]
  (let [v (vec s)]
    (concat (subvec v 0 idx) [e] (subvec v idx))))

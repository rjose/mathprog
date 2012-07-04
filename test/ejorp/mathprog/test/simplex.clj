(ns ejorp.mathprog.test.simplex
  (:require [ejorp.mathprog.simplex :as simplex] )
  (:use clojure.test midje.sweet))

;; ## Fixtures
(def row1 {:val 24, :basic-idx 3, :coeffs [0.5 2 1 1 0]})
(def row2 {:val 60, :basic-idx 4, :coeffs [1 2 4 0 0]})
(def row3 {:val 60, :basic-idx 4, :coeffs [1 -2 4 0 0]})
(def objective {:val 0, :coeffs [6 14 13 0 0]})
(def constraints [row1 row2])
(def problem {:objective objective, :constraints constraints})
(def solution {:status :optimal, :objective {:val -294.0, :coeffs [0.0 -9 0.0 -11.0 0.0]},
               :constraints [{:basic-idx 0, :val 36, :coeffs [1.0 6 0.0 4.0 0.0]}
                             {:basic-idx 2, :val 6, :coeffs [0.0 -1.0 1.0 -1.0 0.0]}]})

(def dual-tableau1
  {
   :objective {:val 0, :coeffs [-3 -1 0 0]},
   :constraints [{:val -1, :basic-idx 2, :coeffs [-1 -1 1 0]}
                 {:val -2, :basic-idx 3, :coeffs [-2 -3 0 1]}]})

(def unbounded-problem {:objective {:val 0, :coeffs [0 0 3 -1]}
                        :constraints [{:basic-idx 0, :val 6, :coeffs [1 0 -3 3]}
                                      {:basic-idx 1, :val 4, :coeffs [0 1 -8 4]}]})

;; ## Tests
(fact (simplex/pivot-row-idx [row1 row2] 1) => 0 )
(fact (simplex/pivot-row-idx [row1 row2 row3] 1) => 0 )
(fact (simplex/pivot-row-idx [row3] 1) => nil)

;; TODO: Figure out how to indicate an unbounded result
;; TODO: Figure out if an LP is in canonical form


(facts "about tableau-status"
  (simplex/tableau-status problem)  => nil
  (simplex/tableau-status solution) => :optimal
  (simplex/tableau-status unbounded-problem) => :unbounded)

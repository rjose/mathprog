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

;; ## Tests
(fact (simplex/pivot-row-idx [row1 row2] 1) => 0 )
(fact (simplex/pivot-row-idx [row1 row2 row3] 1) => 0 )
(fact (simplex/pivot-row-idx [row3] 1) => nil)

;; TODO: Figure out how to indicate an unbounded result
;; TODO: Figure out if an LP is in canonical form

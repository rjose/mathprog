(ns ejorp.mathprog.test.sensitivity
  (:require [ejorp.mathprog.sensitivity :as sensitivity] )
  (:use clojure.test midje.sweet))

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

;; ## Tests
(facts "about shadow prices"
  (let [shadow-prices (sensitivity/shadow-prices start-tableau1 end-tableau1)]
    (nth shadow-prices 0) => (roughly 0.786)
    (nth shadow-prices 1) => (roughly 0.0286)
    (nth shadow-prices 2) => (roughly 0.0)))

(facts "about reduced costs"
  (let [reduced-costs (sensitivity/reduced-costs start-tableau1 end-tableau1)]
    (nth reduced-costs 0) => (roughly 0)
    (nth reduced-costs 1) => (roughly 0)
    (nth reduced-costs 2) => (roughly 0.571)))

(facts "about objective coefficient sensitivity"
  (let [obj-sensitivity (sensitivity/obj-coeff-limits start-tableau1 end-tableau1)]
    (-> obj-sensitivity (nth 0) :gt) => (roughly 4.64)
    (-> obj-sensitivity (nth 0) :lt) => (roughly 5.4)
    
    (-> obj-sensitivity (nth 1) :lt) => (roughly 6.5)
    (-> obj-sensitivity (nth 1) :gt) => (roughly 4.17)

    (-> obj-sensitivity (nth 2) :gt) => nil
    (-> obj-sensitivity (nth 2) :lt) => (roughly 6.57)))


(ns ^{:doc "doc-string"
      :author "Kapil Reddy <reddy.kapil@gmail.com>"}
  thinkstats-clj.c-2
  (:require [thinkstats-clj.core :as core]
            [thinkstats-clj.c-1 :as c1]))

;; Exercise 1
(defn pumpkin
  [xs]
  {:mean (core/mean xs)
   :variance (core/variance xs)
   :std-dev (core/std-dev xs)})


(defn ex-2
  []
  (let [[first-births non-first-births] (c1/ex-3-3)]
    [(core/std-dev (map :prglength first-births))
     (core/std-dev (map :prglength non-first-births))]))

;; @TODO plot histogram

(defn ex-4
  [xs age]
  (let [pfm (core/pmf xs)
        new-pfm (reduce conj
                        {}
                        (filter (fn [[k v]]
                                  (< k age))
                                pfm))]
    (core/normalize-pmf new-pfm)))

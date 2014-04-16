(ns ^{:doc "Chapter 1 exercises"
      :author "Kapil Reddy <reddy.kapil@gmail.com>"}
  thinkstats-clj.c-1
  (:require [thinkstats-clj.core :as core]))

(defn ex-3-1
  []
  (core/load-data :2002-fem-preg))

(comment (count (ex-3-1)))  ;; Count of all pregnancy records


(defn ex-3-2
  []
  (filter #(= (:outcome %) 1)
          (ex-3-1)))

(comment (count (ex-3-2))) ;; Count of all live births


(defn ex-3-3
  []
  (let [first-births (filter #(= (:birthord %) 1)
                             (ex-3-2))
        non-first-births (filter #(not (= (:birthord %) 1))
                                 (ex-3-2))]
    [first-births non-first-births]))

(comment (map count (ex-3-3))) ;; Count first born babies and non first born babies
(comment (reduce + (map count (ex-3-3)))) ;; Count of all live births

(defn ex-3-4
  []
  (let [[first-births non-first-births] (ex-3-3)
        avg-preg-calc #(/ (reduce + (map :prglength %))
                          (count %))]
    (float (* 24 7 (- (avg-preg-calc first-births)
                      (avg-preg-calc non-first-births))))))

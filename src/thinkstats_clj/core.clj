(ns ^{:doc "Utility functions"
      :author "Kapil Reddy <reddy.kapil@gmail.com>"}
  thinkstats-clj.core
  (:require [clojure.java.io :refer [resource] :as io])
  (:import [java.util.zip GZIPInputStream]
           [java.io FileInputStream File]))


(defn parse-field
  [in start end type]
  (let [field (clojure.string/trim (subs in (dec start) end))]
     (when (seq field)
       (condp = type
         :int (Integer/parseInt field)
         :float (Float/parseFloat field)))))

(defn parse-line
  [line]
  {:caseid (parse-field line 1 12 :int)
   :prglength (parse-field line 275 276 :int)
   :outcome (parse-field line 277 277 :int)
   :birthord (parse-field line 278 279 :int)
   :finalwgt (parse-field line 423 440 :float)})


(defn load-data
  [type]
  (let [file-url  (condp = type
                    :2002-fem-preg (resource "2002FemPreg.dat.gz")
                    :2002-fem-resp (resource "2002FemResp.dat.gz"))]
    (map parse-line
         (line-seq (io/reader (GZIPInputStream. (FileInputStream. (io/as-file file-url))))))))


(defn mean
  [xs]
  (float (/ (reduce + xs)
            (count xs))))


(defn variance
  [xs & [mu]]
  (let [mu (when-not mu
             (mean xs))]
    (mean (map #(Math/pow (- % mu) 2) xs))))


(defn std-dev
  [xs]
  (Math/sqrt (variance xs)))


(defn hist
  [xs]
  (reduce (fn [op i]
            (update-in op [i] (fnil inc 0)))
          {}
          xs))


(defn pmf
  [xs]
  (let [len (count xs)
        h (hist xs)]
    (reduce conj
            {}
            (map (fn [[k v]]
                   [k (float (/ v len))])
                 h))))

(defn mode
  [h]
  (apply max (vals h)))


(defn modes
  [h]
  (sort-by (comp - val) h))


(defn normalize-pmf
  [p]
  (let [total (reduce + (vals p))]
    (if-not (zero? total)
      (reduce conj
              {}
              (map (fn [[k v]]
                     [k (/ v total)])
                   p))
      p)))

;; Ex-6
(defn pmf-mean
  [p]
  (reduce (fn [op [k v]]
            (+ op (* k v)))
          0
          p))


(defn pmf-var
  [p]
  (let [m (pmf-mean)]
    (reduce +
            (map (fn [[k v]]
                   (* (Math/pow (- k m) 2)
                      v))
                 p))))

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

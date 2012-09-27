(ns mochi.test.span
  (:require [mochi.span :as span])
  (:use [clojure.test]))

(deftest basic-test
  (are [x y] (= x y)
       (span/start [1 2]) 1
       (span/stop [1 2]) 2
       (span/length [1 5]) 4

       (span/contains? [1 3] [2 4]) false
       (span/contains? [1 5] [2 4]) true
       
       (span/intersect [1 3] [2 4]) [2 3]
       (span/intersect [1 5] [2 4]) [2 4]
       
       (span/from-elems 1 2 10) [1 11]))
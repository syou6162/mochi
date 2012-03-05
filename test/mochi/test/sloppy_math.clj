(ns mochi.test.ml.naive-bayes
  (:use [mochi.sloppy-math]
	[clojure.test]))

(deftest test-log-add
  (is (= 6.0 (Math/exp (log-add [0 0 0 0 0 0])))))

(deftest test-digamma
  (is (= (digamma 1)
	 -0.5772156051517745)))

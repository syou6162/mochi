(ns mochi.test.counter
  (:require [mochi.counter :as cntr])
  (:use [clojure.test]))

(deftest test-make
  (is (= 0.0 (cntr/get-count (cntr/make) :a)))
  (is (= 1 (cntr/get-count (cntr/make {:a 1, :b 2}) :a)))
  (is (= 0.0 (cntr/get-count (cntr/make {:a 1, :b 2}) :c)))
  (is (= 3 (cntr/total-count (cntr/make {:a 1, :b 2})))))

(deftest test-map-counter
  (is (= (.counts
	  (cntr/map-counter inc (cntr/make {:a 1, :b 2})))
	 {:a 2, :b 3}))
  (is (= (.counts
	  (cntr/map-counter #(* 2 %) (cntr/make {:a 1, :b 2})))
	 {:a 2, :b 4})))

(deftest test-scale
  (is (= (.counts
	  (cntr/scale (cntr/make {:a 1, :b 2}) 0.1))
	 {:a 0.1, :b 0.2})))

(deftest test-normalize
  (is (= (.counts
	  (cntr/normalize (cntr/make {:a 1, :b 2})))
	 {:a (/ 1 3), :b (/ 2 3)})))

(deftest test-merge-counters
  (is (= (.counts
	  (cntr/merge-counters (cntr/make {:a 1, :b 2})
			       (cntr/make {:a 1, :c 2})))
	 {:a 2, :b 2, :c 2})))

(deftest test-log-normalize-and-log-scores-to-probs
  (let [cnt (cntr/make {:a (Math/log 0.3),
			:b (Math/log 0.7)})]
    (let [[log-sum normalized-cnt] (cntr/log-normalize cnt)]
      (is (= '(0.29999999999999993 0.7)
	     (map (fn [[k v]] (Math/exp v)) (.counts normalized-cnt)))))
    (let [[_ cnt] (cntr/log-scores-to-probs cnt)]
      (is (= '(0.29999999999999993 0.7)
	     (map second (.counts cnt)))))))

(deftest test-find-max
  (is (= [:b 2] (cntr/find-max (cntr/make {:a 1, :b 2})))))

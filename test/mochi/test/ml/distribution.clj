(ns mochi.test.ml.distribution
  (:use
   [mochi.ml.distribution]
   [clojure.test]))

(deftest test-make-DirichletMultinomial
  (let [dm (make-DirichletMultinomial :lambda 1.0)]
    (is (= 1.0 (.lambda dm))))
  (let [dm (make-DirichletMultinomial
	    :lambda 1
	    :counts {:a 1
		     :b 3
		     :c 6})]
    (is (= (/ (+ 1 1) (+ 10 3))
	   (prob dm :a)))
    (is (= (/ (+ 3 1) (+ 10 3))
	   (prob dm :b)))
    (is (= (/ (+ 6 1) (+ 10 3))
	   (prob dm :c)))))

(deftest test-sample
  (let [prob {:a 0.2
	      :b 0.5
	      :c 0.3}
	dm (make-DirichletMultinomial
	    :counts prob)
	N 100000
	r (java.util.Random. 12345)
	samples (for [_ (range N)] (sample dm r))
	table (frequencies samples)
	supp (set (support dm))]
    (dorun (map #(is (contains? supp %)) samples))
    ;; Chi_square_test with K – 1 degrees of freedom
    ;; http://kusuri-jouhou.com/statistics/bunpuhyou.html
    (is (< (reduce + (for [key supp]
                       (let [Ok (table key)
                             Ek (/ (* N (prob key)) 1.0)]
                         (/ (* (- Ok Ek) (- Ok Ek)) Ek))))
           9.488))))

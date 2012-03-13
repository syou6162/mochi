(ns mochi.test.ml.nested-crp
  (:require [mochi.ml.nested-crp :as crp])
  (:use [clojure.test]))

(deftest test-node-ifn
  (let [inner (crp/make-Node
	       :level 1
	       :numOfCus 1
	       :children nil)
	outer (crp/make-Node
	       :level 0
	       :numOfCus 1
	       :children
	       {0 inner})]
    (is (= inner (outer 0)))
    (is (= inner (get outer 0)))
    (is (= inner (get-in outer [0])))
    (is (= (crp/make-Node
	    :level 2
	    :numOfCus 1
	    :children nil)
	   (get-in outer [0 0])))
    (is (= (crp/make-Node
	    :level 3
	    :numOfCus 1
	    :children nil)
	   (get-in outer [0 0 0])))))

(deftest test-add-doc
  (let [tree (crp/make-Node
	      :level 0
	      :numOfCus 1
	      :children {1 (crp/make-Node
			    :level 1
			    :numOfCus 1
			    :children {3 (crp/make-Node
					  :level 2
					  :numOfCus 1
					  :children nil)})
			 2 (crp/make-Node
			    :level 1
			    :numOfCus 1
			    :children nil)})]
    (is (= (crp/inc-cus-num tree [1 3])
	   (crp/make-Node
	      :level 0
	      :numOfCus 2  ;; changed here!!!
	      :children {1 (crp/make-Node
			    :level 1
			    :numOfCus 2 ;; changed here!!!
			    :children {3 (crp/make-Node
					  :level 2
					  :numOfCus 2 ;; changed here!!!
					  :children nil)})
			 2 (crp/make-Node
			    :level 1
			    :numOfCus 1
			    :children nil)})))
    (is (= (crp/inc-cus-num tree [1 4])
	   (crp/make-Node
	      :level 0
	      :numOfCus 2 ;; added this node!!!
	      :children {1 (crp/make-Node
			    :level 1
			    :numOfCus 2 ;; added this node!!!
			    :children {3 (crp/make-Node
					  :level 2
					  :numOfCus 1
					  :children nil)
				       4 (crp/make-Node ;; added this node!!!
					  :level 2
					  :numOfCus 1
					  :children nil)})
			 2 (crp/make-Node
			    :level 1
			    :numOfCus 1
			    :children nil)})))))

(deftest test-del-doc
  (let [tree (crp/make-Node
	      :level 0
	      :numOfCus 2
	      :children {1 (crp/make-Node
			    :level 1
			    :numOfCus 2
			    :children {3 (crp/make-Node
					  :level 2
					  :numOfCus 2
					  :children nil)})
			 2 (crp/make-Node
			    :level 1
			    :numOfCus 2
			    :children nil)})]
    (is (= (crp/dec-cus-num tree [1 3])
	   (crp/make-Node
	      :level 0
	      :numOfCus 1
	      :children {1 (crp/make-Node
			    :level 1
			    :numOfCus 1
			    :children {3 (crp/make-Node
					  :level 2
					  :numOfCus 1
					  :children nil)})
			 2 (crp/make-Node
			    :level 1
			    :numOfCus 2
			    :children nil)})))))

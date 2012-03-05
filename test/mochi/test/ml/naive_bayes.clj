(ns mochi.test.ml.naive-bayes
  (:use
   [mochi.sloppy-math]
   [mochi.ml.naive-bayes]
   [clojure.test]))

(deftest test-classify
  (let [data [[:ham [["sex" 1] ["hello" 1] ["viagra" 1] ["spam" 1]]]
	      [:spam [["spam1" 1] ["spam2" 1]]]
	      [:spam [["spam1" 1] ["spam3" 1] ["sex" 1]]]
	      [:ham [["sex" 1] ["hello" 1] ["viagra" 1]]]
	      [:ham [["sex" 1] ["hello" 1] ["viagra" 1]]]]
	params (learn data)]
    (is (= :ham (classify params [:ham [["sex" 1] ["hello" 1] ["hiiiiiiiiiii" 1]]])))
    (is (= :spam (classify params [:spam [["spam1" 1] ["spam2" 1] ["sex" 1]]])))))

(ns mochi.test.tree
  (:require [mochi.tree :as tree])
  (:use [clojure.test]))

(deftest leaf-test
  (is (= true (tree/leaf? ["aaa"])))
  (is (= false (tree/leaf? ["aaa" [""]])))

  (is (= false (tree/pre-leaf? ["aaa"])))
  (is (= true (tree/pre-leaf? ["aaa" ["x"]])))
  (is (= false (tree/pre-leaf? ["aaa" ["x"] ["y"]]))))

(deftest nodes-test
  (is (= (tree/nodes ["a" ["b" ["c"] ["d" ["e"]]] "f"])
         [["a" ["b" ["c"] ["d" ["e"]]] "f"]
          ["b" ["c"] ["d" ["e"]]]
          ["c"]
          ["d" ["e"]]
          ["e"]
          "f"])))

(deftest leaves-test
  (is (= (tree/leaves
          ["S" ["NP" ["DT" "the"] ["JJ" "quick"] ["JJ" "brown"]
                ["NN" "fox"]]	  
           ["VP" ["VBD" "jumped"] 
            ["PP" ["IN" "over"] 
             ["NP" ["DT" "the"] ["JJ" "lazy"] ["NN" "dog"]]]]])
         '("the" "quick" "brown" "fox" "jumped"
           "over" "the" "lazy" "dog"))))

(deftest pre-leaves-test
  (is (= (tree/pre-leaves
          ["S" ["NP" ["DT" "the"] ["JJ" "quick"] ["JJ" "brown"]
                ["NN" "fox"]]	  
           ["VP" ["VBD" "jumped"] 
            ["PP" ["IN" "over"] 
             ["NP" ["DT" "the"] ["JJ" "lazy"] ["NN" "dog"]]]]])
         '(["DT" "the"] ["JJ" "quick"] ["JJ" "brown"] ["NN" "fox"]
             ["VBD" "jumped"] ["IN" "over"] ["DT" "the"]
               ["JJ" "lazy"] ["NN" "dog"]))))

(deftest yield-test
  (is (= (tree/yield
          ["S" ["NP" ["DT" "the"] ["JJ" "quick"] ["JJ" "brown"]
                ["NN" "fox"]]	  
           ["VP" ["VBD" "jumped"] 
            ["PP" ["IN" "over"] 
             ["NP" ["DT" "the"] ["JJ" "lazy"] ["NN" "dog"]]]]])
         '("the" "quick" "brown" "fox" "jumped" "over"
           "the" "lazy" "dog"))))

(deftest pre-yield-test
  (is (= (tree/pre-yield
          ["S" ["NP" ["DT" "the"] ["JJ" "quick"] ["JJ" "brown"]
                ["NN" "fox"]]	  
           ["VP" ["VBD" "jumped"] 
            ["PP" ["IN" "over"] 
             ["NP" ["DT" "the"] ["JJ" "lazy"] ["NN" "dog"]]]]])
         '("DT" "JJ" "JJ" "NN" "VBD" "IN" "DT" "JJ" "NN"))))

;; 共通の親を持つ中で一番深いレベルにいるものをTreeで返す
(deftest find-lca-test
  (is (= (tree/find-lca (tree/add-ids
                         ["S" ["NP" ["DT" "the"] ["JJ" "quick"] ["JJ" "brown"]
                               ["NN" "fox"]]	  
                          ["VP" ["VBD" "jumped"] 
                           ["PP" ["IN" "over"] 
                            ["NP" ["DT" "the"] ["JJ" "lazy"] ["NN" "dog"]]]]])
                        [2 3 0] ; brown
                        [1 2 0] ; quick
                        )
         #mochi.tree.Tree{:_label "NP",
                          :_children [#mochi.tree.Tree{:_label "DT",
                                                       :_children [#mochi.tree.Tree{:_label "the", :_children [], :id [0 1 0]}], :id [0 1 1]}
                                      #mochi.tree.Tree{:_label "JJ", :_children [#mochi.tree.Tree{:_label "quick", :_children [], :id [1 2 0]}],
                                                       :id [1 2 1]}
                                      #mochi.tree.Tree{:_label "JJ",
                                                       :_children [#mochi.tree.Tree{:_label "brown", :_children [], :id [2 3 0]}],
                                                       :id [2 3 1]}
                                      #mochi.tree.Tree{:_label "NN",
                                                       :_children [#mochi.tree.Tree{:_label "fox", :_children [], :id [3 4 0]}],
                                                       :id [3 4 1]}],
                          :id [0 4 2]})))
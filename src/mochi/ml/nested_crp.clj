(ns mochi.ml.nested-crp
  (:use [clojure.contrib.str-utils :only (str-join)]
	[clojure.contrib.def :only (defnk)]))

(deftype Node [level numOfCus children]
  clojure.lang.ILookup
  (valAt [this id not-found] (let [default-node (if (nil? not-found)
						  (Node. (inc level) 1 nil)
						  not-found)
				   c (.children this)]
			       (cond (nil? c) default-node
				     (contains? c id) (c id)
				     :else default-node)))
  (valAt [this id] (.valAt this id nil))
  clojure.lang.IFn
  (invoke [this id] (.valAt this id))
  clojure.lang.Associative
  (assoc [this key val] (Node. level
			       numOfCus
			       (assoc (.children this) key val)))
  java.lang.Object
  (equiv [this other] (and
		       (= level (.level other))
		       (= numOfCus (.numOfCus other))
		       (= children (.children other))))
  (toString [this] (str
		    "Level: " level ", "
		    "NumOfCus: " numOfCus ", "
		    "Children: #<" 
		    "\n"
		    (str-join ", \n"
			      (map (fn [[id node]]
				     (str
				      (apply str (map (fn [_] "\t") (range 1 (+ 2 level))))
				      "#<Node " node ">")) children))
		    ">")))

(defnk make-Node
  [:level 0 :numOfCus 0 :children nil]
  (Node. level numOfCus children))




(def tree (make-Node
	   :level 0
	   :numOfCus 2
	   :children {1 (make-Node
			 :level 1
			 :numOfCus 2
			 :children {3 (make-Node
				       :level 2
				       :numOfCus 2
				       :children nil)})
		      2 (make-Node
			 :level 1
			 :numOfCus 2
			 :children nil)}))

(defn map-path [node f path]
  (let [level (:level node)
	numOfCus (:numOfCus node)
	children (:children node)]
    (if (empty? path)
      (Node. level (f numOfCus) children)
      (Node. level (f numOfCus) (assoc children
			  (first path)
			  (map-path (get children (first path)) f (rest path)))))))

; (map-path tree inc '[1])



(defn inc-num [tree xs]
  (let [before (get-in tree xs)
	level (.level before)
	children (.children before)
	after (Node. level (inc (.numOfCus before)) children)]
    (assoc-in tree xs after)))

(defn dec-num [tree xs]
  (let [before (get-in tree xs)
	level (.level before)
	children (.children before)
	after (Node. level (dec (.numOfCus before)) children)]
    (assoc-in tree xs after)))

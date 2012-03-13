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

(defn- change-cus-num [tree f path make-children-func]
  (let [level (.level tree)
	numOfCus (.numOfCus tree)
	children (.children tree)]
    (if (empty? path)
      (Node. level (f numOfCus) children)
      (Node. level (f numOfCus) (make-children-func level children path)))))

(defn inc-cus-num [tree path]
  (letfn [(make-children-func [level children path]
			      (assoc children
				(first path)
				(inc-cus-num (get children (first path)
						  (Node. (inc level) 0 nil))
					     (rest path))))]
    (change-cus-num tree inc path make-children-func)))

(defn dec-cus-num [tree path]
  (letfn [(make-children-func [level children path]
			      (if (nil? (get children (first path)))
				children
				(assoc children
				  (first path)
				  (dec-cus-num (get children (first path))
					       (rest path)))))]
    (change-cus-num tree dec path make-children-func)))

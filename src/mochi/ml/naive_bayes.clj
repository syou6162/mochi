(ns mochi.ml.naive-bayes
  (:require 
   [mochi.core :as mc]
   [mochi.sloppy-math :as ss]
   [mochi.counter :as cntr]
   [mochi.ml.distribution :as distr])
  (:use
   [clojure.contrib.duck-streams :only (reader read-lines)]
   [clojure.contrib.string :only (split)]))

;;; Data ;;;

(defprotocol IDatum
  "Data supports a method to get at [f,v] pairs"
  (feat-vec [d] "vec of [f,v] pairs"))

(defprotocol ILabeled
  "Something Labeled just supports a label method"
  (label [d] "label of the datum"))

;; Default Implementation assumes vector [label feat-vec]
(extend-type clojure.lang.IPersistentVector
  IDatum
  (feat-vec [x] (second x))
  ILabeled
  (label [x] (first x)))

;;;  Params ;;;

(defrecord Params [labelToFeatDistrs labelPrior])

(def default-feat-distr
     (let [prob-of-unk 1.0e-20]
       (reify
	distr/IDistribution
	(log-prob [this e] (Math/log prob-of-unk)))))

(defn all-labels [params] (distr/support (.labelPrior params)))

(defn feat-distr [params label]
  (fn [f]
    (let [distr (-> params :labelToFeatDistrs (get label))]
      (if (contains? (.counts (-> distr .counts)) f)
	distr default-feat-distr))))

(defn obs-log-prob
  "given datum, f: label -> log P(datum|label)"
  [params datum]
  (let [fvs (feat-vec datum)]
    (fn [label]
      (mc/sum (fn [[f v]]
		(+ v (distr/log-prob ((feat-distr params label) f) f)))
	      fvs))))

(defn log-prior
  "f: label -> log P(label)"
  [params]
  (fn [label]
    (distr/log-prob (:labelPrior params) label)))

(defn datum-log-prob
  "given datum, f: label -> log P(datum,label)"
  [params datum]
  (let [prior (log-prior params)
	obs (obs-log-prob params datum)]
    (fn [label] (+ (prior label) (obs label)))))

(defn posteriors
  "given datum, f: label -> log P(label | datum)"
  [params datum]
  (let [labels (all-labels params)
	ll (datum-log-prob params datum)
	log-sum (ss/log-add (map ll labels))]
    (mc/make-map #(Math/exp (- (ll %) log-sum)) labels)))

(defn classify [params test-example]
  (let [labels (all-labels params)
	ll (datum-log-prob params test-example)
	result (mc/make-map ll labels)]
    (first (first (sort-by second > result)))))

;;; -----------------
;;; Learn / Training
;;; -----------------

(defn- make-label-prior [data]
  (reduce 
   (fn [d [label datums]] (distr/obs d label (count datums)))
   (distr/make-DirichletMultinomial :lambda 1.0)
   (group-by label data)))

(defn make-feat-distrs
  "returns a map feat => distr"
  [fvs]
  (let [v fvs
	grp (group-by first v)
	prob-pairs (->> grp
			(map (fn [[k fvs]]
			       (list k (reduce + (map second fvs)))))
			(flatten))]
    (apply
     distr/obs-all
     (distr/make-DirichletMultinomial :lambda 1.0)
     prob-pairs)))

(defn- feat-distrs-factory
  [data]
  (let [data-by-label (group-by label data)]
    (fn [label]
      (make-feat-distrs (mapcat feat-vec (data-by-label label))))))

(defn learn [data]
  (let [prior (make-label-prior data)]
    (Params.
     (mc/make-map (feat-distrs-factory data) (distr/support prior))
     prior)))

(defn -main [& args]
  (letfn [(parse-line [line] (let [[y fv] (-> (re-seq #"([-+]?1)\s(.*)" line)
					      (first)
					      (rest))]
			       [(if (= y "+1") 1 -1)
				(->> fv
				     (split #"\s")
				     (map #(let [[xi cnt] (split #":" %)]
					     [xi (Double/parseDouble cnt)]))
				     (vec))]))
	  (get-f-value [gold prediction] (let [freq (frequencies (map vector gold prediction))
					       tp (get freq [1 1] 0)
					       tn (get freq [-1 -1] 0)
					       fp (get freq [-1 1] 0)
					       fn (get freq [1 -1] 0)
					       recall (/ tp (+ tp fn))
					       precision (/ tp (+ tp fp))]
					   (/ (* 2.0 recall precision)
					      (+ recall precision))))
	  (read-examples [filename] (vec (remove nil? (for [line (read-lines filename)]
							(try (parse-line line)
							     (catch Exception e nil))))))]
    (let [train-examples (read-examples "train.txt")
	  test-examples (take 1000 (read-examples "test.txt"))
	  gold (map label test-examples)
	  params (learn train-examples)]
      (println (get-f-value gold (map #(classify params %) test-examples))))))

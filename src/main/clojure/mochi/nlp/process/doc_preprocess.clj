(ns mochi.nlp.process.doc-preprocess
  (:use [mochi file-utils core vec-utils cli]
	      [mochi.nlp.process parser tokenizer sent-splitter])
  (:require [clojure.contrib
     [duck-streams :as ds] [seq-utils :as su]]))

(defn process
  "returns seq of Sentence objects"
  [txt]
  (ensure-vec 
   (for [[start stop] (sent-spans txt)
	 :let [sent-txt (.substring txt start stop)
	       toks (map (fn [t] 
			  (let [[tok-start tok-stop] (:char-span t)]
			    (assoc t :abs-char-span [(+ tok-start start)
						     (+ tok-stop start)])))
			(tokenize sent-txt))]]
        (do #_(break)
	    (make-Sentence (ensure-vec toks) [start stop] txt)))))

(defn add-parses
  ([sents words-fn]
    (for [s sents] (assoc s :tree (apply parse-sent (words-fn s)))))
  ([sents]
     (add-parses sents (fn [s] (map :word (:toks s))))))

(defn main []
  (let [[opts [inputList outputDir & _]]
	(parse-cmd-line *command-line-args*
			[:parse false "add parses?" false]
			[:outExt false "output ext" ".tok"]
			[:outParseExt false "output trees" ".tok"])]
    (when outputDir (.mkdir (java.io.File. outputDir)))
    (letfn [(out-file [f] (if outputDir
			    (-> f 
			      (change-dir outputDir)
			      (change-ext (:outExt  opts)))
			    (change-ext f (:outExt opts))))]
      (doseq [f (ds/read-lines inputList) ]
	(ds/write-lines
	 (out-file f)
	 (mapcat
	  (fn [[i sent]]
	    (for [[j tok] (su/indexed (:toks sent))]
	      (format "%s %d %d %d %d %d %d" (:raw-word tok) i j
		      (first (:abs-char-span tok)) (second (:abs-char-span tok))
		      (first (:char-span tok)) (second (:char-span tok)))))		      
	  (su/indexed (process (slurp f)))))))))  


(when *command-line-args* (main))

(comment
  (use 'mochi.nlp.process.parser)
  (tokenize "Aria is cool.")
  (def a  (process "Aria is cool. Isn't he?"))
  (type (first (first a)))
  (add-parses (process "Aria is cool. Isn't he?"))
  (for [s (process "Aria is cool. Isn't he?")]
    (str (apply parse-sent (map :word (:toks s)))))    
)

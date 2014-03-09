; Copyright (c) Jayaraj Poroor. All rights reserved.
; The use and distribution terms for this software are covered by the
; GNU Lesser General Public License 3.0 
; (http://www.gnu.org/copyleft/lesser.html)
; which can be found in the file lgpl-3.0.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns generuse.lib.exec
	(:gen-class)
	(:import [clojure.lang IPersistentMap IPersistentVector IPersistentList]
			 [clojure.lang LazySeq IChunkedSeq]
	)	
)

(defmacro defaxon [target-type names fn-form]
	(list 'def 
		  (symbol (str (first names) "_" target-type))		
		  (list 'with-meta
		  		(list
		  			'fn
		  			[^IPersistentMap 'target-eval 
		  			 ^IPersitentMap 'param-evals 
		  			 ^IPersistentMap 'ctx 
		  			 ^IPersistentMap 'globals
		  			]
		  			fn-form		  			
		  		)
		  		{:axon {:names names 
		  			    :target-type (when (not= target-type :any) target-type)
		  			   }
		  	    }
		  )
	)
)

(def is-numeric-type? (memoize (fn [t]
		(or (= t Double ) (= t Long) (= t clojure.lang.Ratio))
	))
)

(def is-basic-type? (memoize (fn [t]
		(or (is-numeric-type? t) (= t Boolean) (= t String) (= :nil t))
	))
)

(defn basic-type-keyword [t]
	(condp = t
		Long 				:integer
		String 				:string
		Boolean 			:boolean
		Double  			:float
		clojure.lang.Ratio  :ratio
		:nil                :nil
		nil
	)
)

(defn classify-type[v]
	(if (or (string? v) (integer? v) (float? v) (true? v) (false? v) (ratio? v))
		:primitive
		(class v)
	)
)

(defmulti to-eval classify-type)

(defmethod to-eval IPersistentMap [v]
	(assoc
		(apply merge
			(map           ;name converts keyword (from sql query) to string
				#(hash-map (name (first %)) (-> % second to-eval))
				v
			)
		)
	    :type :map 
		:mode "strict"
	)
)

(defmethod to-eval nil [v]
	{
		:type 	:nil
		:value 	nil
	}
)

(defmethod to-eval LazySeq [v]
	{
		:value (map #(to-eval %) v)
		:type :seq
		:mode "strict"
	}
)

(defmethod to-eval IChunkedSeq [v]
	{
		:value (map #(to-eval %) v)
		:type :seq
		:mode "strict"
	}
)

(defmethod to-eval IPersistentVector [v]
	(to-eval (seq v))
)

(defmethod to-eval IPersistentList [v]
	(to-eval (seq v))
)

(defmethod to-eval :primitive [v]
	{:value v :type (class v)}
)

(defn deref-eval[eval]
	(if (= (:type eval) :heap-obj) 
	 	@(:value eval)
	 	eval 
	)
)

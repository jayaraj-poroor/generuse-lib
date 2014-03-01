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
	(:import [clojure.lang IPersistentMap])	
)

(defn basic-type-keyword [t]
	(condp = t
		Long 				:int
		String 				:string
		Boolean 			:boolean
		Double  			:float
		clojure.lang.Ratio  :ratio
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
	(apply merge
		(map           ;name converts keyword (from sql query) to string
			#(hash-map (name (first %)) (-> % second to-eval) 
					   :type :map 
					   :mode "strict"
			)
			v
		)
	)
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

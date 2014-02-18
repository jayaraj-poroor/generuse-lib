(ns generuse.lib.exec
	(:gen-class)
	
)

(defn deref-eval[eval]
	(if (= (:type eval) :heap-obj) 
	 	@(:value eval)
	 	eval 
	)
)

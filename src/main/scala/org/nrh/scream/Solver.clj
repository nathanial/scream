
(defn solve [state] 
  (if (all-assigned state)
    (when (all-satisfied state)
      state)
    (let [next-var (variable-selector state)
	  permutations 
	  (map #(assoc state next-var (singleton %))
	       (domain next-var))]
      (map solve 
	   (filter consistent
		   (map propogate-constraints permutations))))))      

       
      
      
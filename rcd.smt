(set-logic ALL_SUPPORTED)

(declare-datatypes
 ((Pair 2))
 ((par (A B) ((pair (fst A) (snd B))))))

(declare-datatypes
 ((Ty 0))
 (((arr (arr_param Ty) (arr_return Ty))
   (rcd (rcd_set (Set (Pair Int Ty)))))))

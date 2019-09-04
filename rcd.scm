(load "cvc4-set-tests.scm")
(load "clpset.scm")

(define (declare-datatypes)
  (fresh ()
    (z/ '(declare-datatypes
          ((Label 0))
          (((a) (b) (c) (d)))))

    (z/ '(declare-datatypes
          ((Pair 2))
          ((par (A B) ((pair (fst A) (snd B)))))))

    (z/ '(declare-datatypes
          ((Ty 0))
          (((arr (arr_param Ty) (arr_return Ty))
            (rcd (rcd_set (Set (Pair Label Ty))))))))))

(define typ
  (lambda (t)
    (z/ `(declare-const ,t Ty))))

(define tf
  (lambda (f)
    (z/ `(declare-const ,f (Pair Label Ty)))))

(define tfs
  (lambda (s)
    (z/ `(declare-const ,s (Set (Pair Label Ty))))))

(define sub
  (lambda (t1 t2)
    (conde
      ((z/== t1 t2))
      ((z/assert `(not (= ,t1 ,t2)))
       (conde
         ((fresh (ta1 tb1 ta2 tb2)
            (typ ta1) (typ tb1) (typ ta2) (typ tb2)
            (z/== `(arr ,ta1 tb1) t1)
            (z/== `(arr ,ta2 tb2) t2)
            (sub ta2 ta1)
            (sub tb1 tb2)))
         ((fresh (r1 r2)
            (tfs r1) (tfs r2)
            (z/== `(rcd ,r1) t1)
            (z/== `(rcd ,r2) t2)
            (sub-rcd r1 r2))))))))

(define sub-rcd
  (lambda (r1 r2)
    (subseto r2 r1) ;; also do depth subtyping
    ))

(test "1"
  (run 1 (q)
    (declare-datatypes)
    (fresh (t1 t2)
      (typ t1) (typ t2)
      (== q (list t1 t2))
      (sub t1 t2)))
  '(((rcd (as emptyset (Set (Pair Label Ty))))
     (rcd (as emptyset (Set (Pair Label Ty)))))))

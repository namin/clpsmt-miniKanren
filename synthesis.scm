(load "mk.scm")
(load "cvc4-driver.scm")
(load "test-check.scm")

(define (assert-exs f exs)
  (conde
    ((== '() exs))
    ((fresh (a d i o)
       (== (cons a d) exs)
       (== (cons i o) a)
       (z/assert `(= (,f ,i) ,o))
       (assert-exs f d)))))

(define (synthesize f exs)
  (fresh ()
    (z/ `(declare-fun ,f (Int) Int))
    (assert-exs f exs)))


(run 1 (q)
  (synthesize q '((1 . 2) (3 . 4) (5 . 6) (6 . 7) (7 . 8))))

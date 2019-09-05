(load "mk.scm")
(load "cvc4-driver.scm")
(load "test-check.scm")

;; following https://barghouthi.github.io/2017/04/24/synthesis-primer/

(define (synthesize q exs)
  (fresh (a b)
    (let ((shape `(+ (* ,a x) ,b)))
      (fresh ()
        (z/ `(declare-const ,a Int))
        (z/ `(declare-const ,b Int))
        (z/ `(assert (forall ((x Int) (y Int))
                             (=> (or ,@(map (lambda (ex)
                                              `(and (= x ,(car ex))
                                                    (= y ,(cdr ex))))
                                            exs))
                                 (= y ,shape)))))
        (== q `(lambda (x) ,shape))))))


(test "syn-inc"
  (run* (q) (synthesize q '((1 . 2) (2 . 3))))
  '((lambda (x) (+ (* 1 x) 1))))

(test "syn-double"
  (run* (q) (synthesize q '((1 . 2) (2 . 4))))
  '((lambda (x) (+ (* 2 x) 0))))

(test "syn-const"
  (run* (q) (synthesize q '((1 . 2) (2 . 2))))
  '((lambda (x) (+ (* 0 x) 2))))

(test "syn-no"
  (run* (q) (synthesize q '((2 . 3) (3 . 2) (4 . 3))))
  '())

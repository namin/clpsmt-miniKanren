(load "z3-driver.scm")
(load "test-check.scm")

(test "1"
  (check-sat
   '((declare-fun x () Int)
     (assert (>= x 0))))
  #t)

(test "2"
  (check-sat
   '((declare-fun x () Int)
     (assert (= x 0))
     (assert (= x 1))))
  #f)

(test "3"
  (get-model
   '((declare-fun x () Int)
     (assert (= x 0))))
  '((x . 0)))

(test "4"
  (check-model-unique
   '((declare-fun x () Int)
     (assert (= x 0)))
   '((x . 0)))
  #t)

(test "5"
  (get-all-models
   '((declare-fun x () Int)
     (declare-fun y () Int)
     (assert (= 3 (* x y))))
   '())
  '(((y . 1) (x . 3))
    ((y . -3) (x . -1))
    ((y . 3) (x . 1))
    ((y . -1) (x . -3))))

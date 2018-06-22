(load "mk.scm")
(load "cvc4-driver.scm")
(load "test-check.scm")

(test "set-1"
  (run* (q)
    (z/ `(declare-fun ,q () (Set Int)))
    (z/assert `(= ,q (singleton 1))))
  '((singleton 1)))

(test "set-2"
  (run* (q)
    (z/ `(declare-fun ,q () (Set Int)))
    (z/assert `(subset ,q (insert 2 (singleton 1)))))
  '((as emptyset (Set Int))
    (singleton 2)
    (singleton 1)
    (union [singleton 1] [singleton 2])))


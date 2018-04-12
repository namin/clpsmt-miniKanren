(load "mk.scm")
(load "z3-driver.scm")
(load "test-check.scm")

(test "1"
  (run* (q)
    (z/fresh
     (lambda (x)
       (z/assert `(= ,x 0)))))
  '(_.0))

(test "2"
  (run* (q)
    (z/fresh
     (lambda (x)
       (fresh ()
         (z/assert `(= ,x 0))
         (z/assert `(= ,x 1))))))
  '())

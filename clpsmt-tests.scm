(load "mk.scm")
(load "z3-driver.scm")
(load "test-check.scm")

(test "1"
  (run* (q)
    (fresh (x)
      (z/var x)
      (z/assert `(= ,x 0))))
  '(_.0))

(test "2"
  (run* (q)
    (fresh (x)
      (z/var x)
      (z/assert `(= ,x 0))
      (z/assert `(= ,x 1))))
  '())

(test "3"
  (run* (q)
    (fresh (x)
     (z/var x)
     (z/assert `(= ,x 0))
     (== x q)))
  '(0))

(test "4"
  (run* (q)
    (z/var q)
    (z/assert `(= ,q 0)))
  '(0))

(define faco
  (lambda (n out)
    (conde ((z/assert `(= ,n 0))
            (z/assert `(= ,out 1)))
           ((z/assert `(> ,n 0))
            (fresh (n-1 r)
              (z/var n-1)
              (z/var r)
              (z/assert `(= (- ,n 1) ,n-1))
              (z/assert `(= (* ,n ,r) ,out))
              (faco n-1 r))))))

(test "faco-7"
  (run 7 (q)
    (fresh (n out)
      (z/var n)
      (z/var out)
      (faco n out)
      (== q `(,n ,out))))
  '((0 1) (1 1) (2 2) (3 6) (4 24) (5 120) (6 720)))

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

(test "3"
  (run* (q)
    (z/fresh
     (lambda (x)
       (fresh ()
         (z/assert `(= ,x 0))
         (== x q)))))
  '(0))

(define faco
  (lambda (n out)
    (conde ((z/assert `(= ,n 0))
            (z/assert `(= ,out 1)))
           ((z/assert `(> ,n 0))
            (z/fresh
             (lambda (n-1)
               (fresh ()
                 (z/assert `(= (- ,n 1) ,n-1))
                 (z/fresh
                  (lambda (r)
                    (fresh ()
                      (z/assert `(= (* ,n ,r) ,out))
                      (faco n-1 r)))))))))))

(test "4"
  (run 7 (q)
    (z/fresh
     (lambda (n)
       (z/fresh
        (lambda (out)
          (fresh ()
            (faco n out)
            (== q `(,n ,out))))))))
  '((0 1) (1 1) (2 2) (3 6) (4 24) (5 120) (6 720)))

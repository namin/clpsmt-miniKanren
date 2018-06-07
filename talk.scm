(load "mk.scm")
(load "z3-driver.scm")
(load "test-check.scm")

(test "basic-1"
  (run* (q)
    (z/assert `(> ,q 0))
    (z/assert `(< ,q 2)))
  '(1))

(test "basic-2"
  (run 3 (q)
    (fresh (x y)
      (== q `(,x ,y))
      (z/assert `(= ,x (+ ,y 1)))))
  '((1 0) (2 1) (3 2)))

(define faco
  (lambda (n out)
    (conde ((z/assert `(= ,n 0))
            (z/assert `(= ,out 1)))
           ((z/assert `(> ,n 0))
            (fresh (n-1 r)
              (z/assert `(= (- ,n 1) ,n-1))
              (z/assert `(= (* ,n ,r) ,out))
              (faco n-1 r))))))

(test "faco-7"
  (run 7 (q)
    (fresh (n out)
      (faco n out)
      (== q `(,n ,out))))
  '((0 1) (1 1) (2 2) (3 6) (4 24) (5 120) (6 720)))

(test "faco-backwards-2"
  (run* (q)
    (faco q 2))
  '(2))

(test "faco-backwards-720"
  (run* (q)
    (faco q 720))
  '(6))

(load "full-interp.scm")

(test "evalo-1"
  (run* (q)
    (evalo '(+ 1 2) q))
  '(3))

(test "evalo-fac-6"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac 6))
           q))
  '(720))

(test "evalo-backwards-fac-6"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac ,q))
           720))
  '(6))

;; remember the quote!
(test "evalo-backwards-fac-quoted-6"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac ',q))
           720))
  '(6))







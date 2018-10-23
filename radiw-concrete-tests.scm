(load "radiw-concrete.scm")
(load "test-check.scm")

(define fact
  `(lam self n
        (if0 (var n)
             (int 1)
             (times (var n)
                    (app (var self)
                         (plus (var n) (int -1)))))))

(define efact
  `(app ,fact (int 5)))


(define fib
  `(lam self n
        (if0 (var n)
             (int 0)
             (if0 (plus (var n) (int -1))
                  (int 1)
                  (plus (app (var self)
                             (plus (var n) (int -1)))
                        (app (var self)
                             (plus (var n) (int -2))))))))

(define efib
  `(app ,fib (int 6)))

(time
  (test "fact-5"
    (run* (q)
      (evalo efact q))
    '((int 120))))

(time
  (test "fib-6"
    (run* (q)
      (evalo efib q))
    '((int 8))))

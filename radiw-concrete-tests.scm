(load "radiw-concrete.scm")
(load "test-check.scm")

;; WEB todo
;;
;; Port CLP(SMT) to faster mk
;;
;; Change radiw abstract interp to allow for any integer, rather than just -1, 0, 1
;;
;; Combine concrete and abstract interps for synthesis (perhaps with type inferencer).

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

(time
 (test "fib-synthesis-1"
   (run 1 (fib)
     (fresh (r)
       (== `(lam self n
                 (if0 (var n)
                      (int 0)
                      (if0 (plus (var n) (int -1))
                           ,r
                           (plus (app (var self)
                                      (plus (var n) (int -1)))
                                 (app (var self)
                                      (plus (var n) (int -2)))))))
           fib))
     (evalo `(app ,fib (int 6)) '(int 8)))
   '((lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (plus (var n) (int -1)))
                     (app (var self) (plus (var n) (int -2))))))))))

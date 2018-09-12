(load "mk.scm")
(load "z3-driver.scm")
(load "test-check.scm")
(load "radi.scm")

(test "radi-int-1"
  (run 2 [q]
    (analyzeo `(int 1) q))
  '((((aval (pos) ()) ()))))

(test "radi-plus-1"
  (run 2 [q]
    (analyzeo `(plus (int 1) (int 2)) q))
  '((((aval (pos) ()) ()))))

(test "radi-app-1"
  (run 2 [q]
    (analyzeo `(app (lam x y (var y)) (int 1)) q))
  '((((aval (pos) ())
      ((x (aval () ((x y (app (lam x y (var y)) (int 1))))))
       (y (aval (pos) ())))))))

(test "radi-if0-1"
  (run 2 [q]
    (analyzeo `(if0 (int 0) (int 1) (int -1)) q))
  '((((aval (pos) ()) ()))))

(test "radi-lam-1"
  (run 2 [q]
    (analyzeo `(lam self n (var n)) q))
  '((((aval () ((self n (var n)))) ()))))

(test "radi-app-2"
  (run 2 [q]
    (analyzeo `(app (lam self n (var n)) (int 1)) q))
  '((((aval (pos) ())
    ((self
       (aval () ((self n (app (lam self n (var n)) (int 1))))))
     (n (aval (pos) ())))))))

(test "radi-loop-1"
  (run 2 [q]
    (analyzeo `(app (lam self n (app (var self) (var n))) (int 1)) q))
  '(()))

(test "radi-loop-1a"
  (run 2 [q]
    (analyzeo `(app (lam self n (if0 (var n) (app (var self) (var n)) (app (var self) (var n)))) (int 1)) q))
  '(()))

(todo "radi-loop-2"
  (run 2 [q]
    (analyzeo `(app (lam self n (if0 (var n) (int 1) (app (var self) (plus (var n) (int -1))))) (int 1)) q))
  '(()))

(define fact
  `(lam self n
        (if0 (var n)
             (int 1)
             (times (var n)
                    (app
                     (var self)
                     (plus (var n)
                           (int -1)))))))

(define efact
  `(app ,fact (int 5)))

(todo "radi-efact-1"
  (run 1 [q]
    (analyzeo efact q))
  '((((aval (pos) ()) ()))))

(load "mkf.scm")
(load "test-check.scm")
(load "radiw.scm")

(test "muo-0"
  (run 2 (q)
    (muo '() '() q))
  '(()))

(test "add-uo-1"
  (run 2 (q)
    (add-uo 'a '(aval (pos) ()) '() q))
  '(((a (aval (pos) ())))))

(test "add-uo-2"
  (run 2 (q)
    (add-uo 'a '(aval (pos) ()) '((a (aval (neg) ()))) q))
  '(((a (aval (pos neg) ())))))
  
(test "muo-1"
  (run 2 (q)
    (muo '((a (aval (pos) ())) (b (aval (neg) ())))
         '((a (aval (neg) ()))) q))
  '(((a (aval (neg pos) ())) (b (aval (neg) ())))))

(define fact
  `(lam self n
        (if0 (var n)
             (int 1)
             (times (var n)
                    (app (var self)
                         (plus (var n) (int -1)))))))

(define efact
  `(app ,fact (int 1)))

(test "radiw-efact"
  (run 2 [q]
    (analyzeo efact q))

  )

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

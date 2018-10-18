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

(test "radiw-step-1"
  (run 2 [q]
    (fresh [v s c]
      (== q `(,v ,s ,c))
      (adivalpo '(int 1) '() '() '() v s c)))
  '(((aval (pos) ()) () (((int 1) (aval (pos) ()))))))

(test "radiw-step-2"
  (run 2 [q]
    (fresh [v s c]
      (== q `(,v ,s ,c))
      (adivalpo '(int 1) '() '(((int 1) (aval (pos) ()))) '() v s c)))
  '(((aval (pos) ()) () (((int 1) (aval (pos) ()))))))

(test "radiw-lffpo-1"
  (run 2 [q]
    (fresh [s c]
      (== q `(,s ,c))
      (lfppo '(int 1) '() '() s c)))
  '((() (((int 1) (aval (pos) ()))))))

(test "radiw-lffpo-2"
  (run 2 [q]
    (fresh [s c]
      (== q `(,s ,c))
      (lfppo '(int 1) '() '(((int 1) (aval (pos) ()))) s c)))
  '((() (((int 1) (aval (pos) ()))))))

(test "radiw-num-1"
  (run 2 [q]
    (analyzeo '(int 1) q))
  '((aval (pos) ())))

(test "radiw-num-2"
  (run 2 [q]
    (analyzeo '(int 0) q))
  '((aval (zer) ())))

(test "radiw-num-3"
  (run 2 [q]
    (analyzeo '(int -1) q))
  '((aval (neg) ())))

;; broken
(test "radiw-efact-0"
  (run 2 [q]
    (analyzeo `(app ,fact (int 0)) q))
  '((aval
     ()
     ((self
       n
       (lam self
            n
            (if0 (var n)
                 (int 1)
                 (times (var n)
                        (app (var self)
                             (plus (var n)
                                   (int -1)))))))))))

;; broken
(test "radiw-efact"
  (run 2 [q]
    (analyzeo efact q))
  '((aval
   ()
   ((self
      n
      (lam self
           n
           (if0 (var n)
                (int 1)
                (times
                  (var n)
                  (app (var self) (plus (var n) (int -1)))))))))))

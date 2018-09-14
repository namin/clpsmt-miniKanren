(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")
(load "test-check.scm")
(load "radif.scm")

(define fact
  `(lam self n
        (if0 (var n)
             (int 1)
             (times (var n)
                    (app (var self)
                         (plus (var n) (int -1)))))))

(define efact
  `(app ,fact (int 1)))


(time
  (test "radi-efact-1"
    (run* [q]
      (analyzeo efact q))
    '((((aval (neg zer pos) ())
        ((self (aval () ((self n
                               (if0 (var n)
                                    (int 1)
                                    (times (var n)
                                           (app (var self)
                                                (plus (var n) (int -1)))))))))
         (n (aval (neg zer pos) ()))))
       ((aval (pos) ())
        ((self (aval () ((self n
                               (if0 (var n)
                                    (int 1)
                                    (times (var n)
                                           (app (var self)
                                                (plus (var n) (int -1)))))))))
         (n (aval (neg zer pos) ()))))))))

(time
  (test "radi-efact-backwards-1"
    (run* [q]
      (analyzeo efact
                '(((aval (neg zer pos) ())
                   ((self (aval () ((self n
                                          (if0 (var n)
                                               (int 1)
                                               (times (var n)
                                                      (app (var self)
                                                           (plus (var n) (int -1)))))))))
                    (n (aval (neg zer pos) ()))))
                  ((aval (pos) ())
                   ((self (aval () ((self n
                                          (if0 (var n)
                                               (int 1)
                                               (times (var n)
                                                      (app (var self)
                                                           (plus (var n) (int -1)))))))))
                    (n (aval (neg zer pos) ())))))))
    '(_.0)))

(time
  (test "radi-efact-backwards-2"
    (run* [q]
      (analyzeo `(app ,fact (int ,q))
                '(((aval (neg zer pos) ())
                   ((self (aval () ((self n
                                          (if0 (var n)
                                               (int 1)
                                               (times (var n)
                                                      (app (var self)
                                                           (plus (var n) (int -1)))))))))
                    (n (aval (neg zer pos) ()))))
                  ((aval (pos) ())
                   ((self (aval () ((self n
                                          (if0 (var n)
                                               (int 1)
                                               (times (var n)
                                                      (app (var self)
                                                           (plus (var n) (int -1)))))))))
                    (n (aval (neg zer pos) ())))))))
    '(1)))

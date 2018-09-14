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
  (test "radi-efact-backwards-1-EZ"
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
  (test "radi-efact-backwards-2-EZ"
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

(time
  (test "radi-efact-backwards-3-EZ"
    (run* [q]
      (analyzeo `(app (lam self n
                           (if0 (var n)
                                (int 1)
                                (times (var ,q)
                                       (app (var self)
                                            (plus (var n) (int -1))))))
                      (int 1))
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
    '(n)))

(time
  (test "radi-efact-backwards-4-EZ"
    (run* [q]
      (analyzeo `(app (lam self n
                           (if0 (var n)
                                (int 1)
                                (,q (var n)
                                       (app (var self)
                                            (plus (var n) (int -1))))))
                      (int 1))
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
    '(times)))

(time
  (test "radi-efact-backwards-5-EZ"
    (run* [q]
      (analyzeo `(app (lam self n
                           (if0 (var n)
                                (int 1)
                                (times (var n)
                                       (app (var self)
                                            (plus (var ,q) (int -1))))))
                      (int 1))
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
    '(n)))

(time
  (test "radi-efact-backwards-6-EZ"
    (run 1 [q]
      (analyzeo `(app (lam self n
                           (if0 (var n)
                                (int 1)
                                (times (var n)
                                       (app (var self)
                                            (plus ,q (int -1))))))
                      (int 1))
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
    '((var n))))


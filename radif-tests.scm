(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")
(load "test-check.scm")
(load "radif.scm")

;;; WEB uh oh!
;;; set-equivo will generate non-sets!!
(test "set-equivo-0"
  (run 10 (q)
    (set-equivo '(a) q))
  '((a)))

(test "set-equivo-1"
  (run 10 (q)
    (set-equivo '(a b) q))
  '((a b) (b a)))


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
  (test "radi-efact-backwards-1-harder"
    (run* [q]
      (fresh (astore1 astore2)
        (analyzeo efact
                  `(((aval (neg zer pos) ())
                     ,astore1)
                    ((aval (pos) ())
                     ,astore2)))))
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
  (test "radi-efact-backwards-2-harder"
    (run* [q]
      (fresh (astore1 astore2)
        (analyzeo `(app ,fact (int ,q))
                  `(((aval (neg zer pos) ())
                     ,astore1)
                    ((aval (pos) ())
                     ,astore2)))))
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
  (test "radi-efact-backwards-3-harder"
    (run* [q]
      (fresh (astore1 astore2)
        (analyzeo `(app (lam self n
                             (if0 (var n)
                                  (int 1)
                                  (times (var ,q)
                                         (app (var self)
                                              (plus (var n) (int -1))))))
                        (int 1))
                  `(((aval (neg zer pos) ())
                     ,astore1)
                    ((aval (pos) ())
                     ,astore2)))))
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

;; Note that 'plus' is an answer, along with the expected 'mult'
(time
  (test "radi-efact-backwards-4-harder"
    (run* [q]
      (fresh (astore1 astore2)
        (analyzeo `(app (lam self n
                             (if0 (var n)
                                  (int 1)
                                  (,q (var n)
                                      (app (var self)
                                           (plus (var n) (int -1))))))
                        (int 1))
                  `(((aval (neg zer pos) ())
                     ,astore1)
                    ((aval (pos) ())
                     ,astore2)))))
    '(plus times)))

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
  (test "radi-efact-backwards-5-harder"
    (run* [q]
      (fresh (astore1 astore2)
        (analyzeo `(app (lam self n
                             (if0 (var n)
                                  (int 1)
                                  (times (var n)
                                         (app (var self)
                                              (plus (var ,q) (int -1))))))
                        (int 1))
                  `(((aval (neg zer pos) ())
                     ,astore1)
                    ((aval (pos) ())
                     ,astore2)))))
    '(n)))

;;; WEB really slow, and only works with run 1, due to problems with set-equivo
(time
  (test "radi-efact-backwards-5-honest"
    (run 1 [q]
      (fresh (astore1 astore2 answer-set)
        (set-equivo `(((aval (pos) ())
                       ,astore2)
                      ((aval (neg zer pos) ())
                       ,astore1))
                    answer-set)
        (analyzeo `(app (lam self n
                             (if0 (var n)
                                  (int 1)
                                  (times (var n)
                                         (app (var self)
                                              (plus (var ,q) (int -1))))))
                        (int 1))
                  answer-set)))
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

(time
  (test "radi-efact-backwards-6-harder"
    (run 1 [q]
      (fresh (astore1 astore2)
        (analyzeo `(app (lam self n
                             (if0 (var n)
                                  (int 1)
                                  (times (var n)
                                         (app (var self)
                                              (plus ,q (int -1))))))
                        (int 1))
                  `(((aval (neg zer pos) ())
                     ,astore1)
                    ((aval (pos) ())
                     ,astore2)))))
    '((var n))))

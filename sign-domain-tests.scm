(load "mk.scm")
(load "z3-driver.scm")
(load "sign-domain.scm")
(load "test-check.scm")

(test "1"
  (run* (q)
    (fresh (s b- b0 b+)
      (s/declare-bito b-)
      (s/declare-bito b0)
      (s/declare-bito b+)
      (s/declareo s)
      (s/has-nego  s b-)
      (s/has-zeroo s b0)
      (s/has-poso  s b+)
      (== q (list s b+ b0 b-))))
  ;; TODO: I think this would be faster
  ;;       if has-...o used conde?
  '((bitvec-101 #t #f #t)
    (bitvec-110 #t #t #f)
    (bitvec-111 #t #t #t)
    (bitvec-010 #f #t #f)
    (bitvec-011 #f #t #t)
    (bitvec-000 #f #f #f)
    (bitvec-001 #f #f #t)
    (bitvec-100 #t #f #f)))

(test "2"
  (run* (s)
    (s/declareo s)
    (s/uniono 'bitvec-110 'bitvec-011 s))
  '(bitvec-111))

(test "3"
  (run* (q)
    (s/declareo q)
    (s/membero 'bitvec-110 q))
  '(bitvec-010 bitvec-100))

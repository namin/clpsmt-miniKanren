(define s/declare-bito
  (lambda (b)
    (z/ `(declare-const ,b Bool))))

(define s/declareo
  (lambda (s)
    (z/ `(declare-const ,s (_ BitVec 3)))))

(define s/haso
  (lambda (p)
    (lambda (s b)
      (z/assert `(= ,s (ite ,b (bvor ,s ,p) (bvand ,s (bvnot ,p))))))))

(define vec-neg 'bitvec-001)
(define s/has-nego
  (s/haso vec-neg))

(define vec-zero 'bitvec-010)
(define s/has-zeroo
  (s/haso vec-zero))

(define vec-pos 'bitvec-100)
(define s/has-poso
  (s/haso vec-pos))

(define s/iso
  (lambda (p)
    (lambda (s)
      (z/assert `(= ,s ,p)))))
(define s/is-nego
  (s/iso vec-neg))
(define s/is-zeroo
  (s/iso vec-zero))
(define s/is-poso
  (s/iso vec-pos))

(define s/uniono
  (lambda (s1 s2 so)
    (z/assert `(= (bvor ,s1 ,s2) ,so))))

(define s/is-bito
  (lambda (b)
    (conde
      ((z/assert `(= ,b ,vec-neg)))
      ((z/assert `(= ,b ,vec-zero)))
      ((z/assert `(= ,b ,vec-pos))))))

(define s/membero
  (lambda (s b)
    (fresh ()
      (z/assert `(= (bvand ,s ,b) ,b))
      (s/is-bito b))))

(define s/alphao
  (lambda (n s)
    (fresh ()
      (conde
        ((z/assert `(< ,n 0))
         (s/is-nego  s))
        ((z/assert `(= ,n 0))
         (s/is-zeroo s))
        ((z/assert `(> ,n 0))
         (s/is-poso  s))))))

;; For example,
;; {−,0}⊕{−}={−} and {−}⊕{+}={−,0,+}.
;; {−}⊗{+,0}={−,0} and  {−,+}⊗{0}={0}

(define s/plus-alphao
  (lambda (s1 s2 so)
    (conde
      ((s/is-zeroo s1)
       (z/assert `(= ,so ,s2)))
      ((s/is-zeroo s2)
       (z/assert `(= ,so ,s1)))
      ((s/is-nego s1)
       (s/is-nego s2)
       (s/is-nego so))
      ((s/is-poso s1)
       (s/is-poso s2)
       (s/is-poso so))
      ((s/is-nego s1)
       (s/is-poso s2)
       (z/assert `(= ,so bitvec-111)))
      ((s/is-poso s1)
       (s/is-nego s2)
       (z/assert `(= ,so bitvec-111))))))

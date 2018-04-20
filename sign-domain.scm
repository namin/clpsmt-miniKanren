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

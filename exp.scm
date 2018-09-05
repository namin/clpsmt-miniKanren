(load "mk.scm")
(load "cvc4-driver.scm")
(load "test-check.scm")

(define z/==
  (lambda (a b)
    (z/assert `(= ,a ,b))))

(define (L)
  (z/
   `(declare-datatypes
     ((L 0))
     (((zero)
       (succ (pred L))
       (plus (a L) (b L))
       (ifz (is_zero L) (yes L) (no L)))))))

(define (L/dec x)
  (z/ `(declare-const ,x L)))

(define (evalo l v)
  (conde
    ((z/== 'zero l) (z/== 0 v))
    ((fresh (x i)
       (L/dec x)
       (z/== `(succ ,x) l)
       (z/== `(+ 1 ,i) v)
       (evalo x i)))))

(test "evalo-0"
  (run* (q) (L) (evalo 'zero q))
  '(0))

(test "evalo-0-backwards"
  (run 1 (q) (L) (L/dec q) (evalo q 0))
  '(zero))

(test "evalo-1"
  (run* (q) (L) (evalo '(succ zero) q))
  '(1))

(test "evalo-1-backwards"
  (run 1 (q) (L) (L/dec q) (evalo q 1))
  '((succ zero)))



(load "mk.scm")
(load "z3-driver.scm")
(load "sign-domain.scm")
(load "test-check.scm")

;;; abstract interpreter, inspired by the interpreter in:
;;;
;;; http://matt.might.net/articles/intro-static-analysis/

;; Tag values produced by the abstract interp, to allow the interp to
;; produce closures, booleans, etc, in addition to bit vectors
;; representing abstract integers.

(define lookupo
  (lambda (x env val)
    (conde
      ((fresh (rest)
         (== `((,x . ,val) . ,rest) env)))
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env)
         (symbolo y)
         (=/= y x)
         (lookupo x rest val))))))

(define evalo
  (lambda (expr env val)
    (conde
      [(symbolo expr) (lookupo expr env val)]
      
      [(numbero expr)
       (fresh (v)
         (s/declareo v)
         (s/z3-alphao expr v)
         (== `(num ,v) val))]

      [(fresh (e1 e2 v1 v2 v)
         (== `(+ ,e1 ,e2) expr)
         ;(s/declareo v1)
         ;(s/declareo v2)
         (s/declareo v)
         ;; is this the ideal ordering?
         (== `(num ,v) val)
         (evalo e1 env `(num ,v1))
         (evalo e2 env `(num ,v2))
         ;; ugh--the call to s/z3-plus-tableo should come before the
         ;; recursive evalo calls.  But s/declareo is causing issues!
         (s/z3-plus-tableo v1 v2 v)
         )]

      [(fresh (e1 e2 v1 v2 v)
         (== `(* ,e1 ,e2) expr)
         ;(s/declareo v1)
         ;(s/declareo v2)
         (s/declareo v)
         ;; is this the ideal ordering?
         (== `(num ,v) val)
         (evalo e1 env `(num ,v1))
         (evalo e2 env `(num ,v2))
         ;; ugh--the call to s/z3-plus-tableo should come before the
         ;; recursive evalo calls.  But s/declareo is causing issues!
         (s/z3-times-tableo v1 v2 v)
         )]

      ;;; hmmm---seems like there is a problem here:
      ;;; if we want to make zero? and if work separately,
      ;;; evalo may return a bit vector or a boolean.
      ;;; How would this work, given the need for s/declareo?
      [(fresh (e1 e2 e3 v1)
         (== `(ifzero ,e1 ,e2 ,e3) expr)
         ;; (s/declareo v1)
         ;; is this the ideal ordering?
         (evalo e1 env `(num ,v1))
         (conde
           [(s/chas-zeroo v1)
            (evalo e2 env val)]
           [(conde
              [(s/chas-poso v1)]
              [(s/chas-nego v1)])
            (evalo e3 env val)]))]

      [(fresh (x e body v env^)
         (== `(let ((,x ,e)) ,body) expr)
         (symbolo x)
         ;; (s/declareo v)
         (== `((,x . ,v) . ,env) env^)
         (evalo e env v)
         (evalo body env^ val))]
      
      )))

#|
;;; Hmmm--no bit pattern?
(test "declaro-1"
  (run* (q)
    (s/declareo q))
  '(_.0))

;;; Hmmm--this seems tricky!
;;; An error would be better
(test "declaro-2"
  (run* (q)
    (s/declareo q)
    (s/declareo q))
  '())

;;; Compare with declareo-1 -- now can see that we have bit patterns.
(test "declaro-3"
  (run* (q)
    (s/declareo q)
    (s/chas-poso q))
  '(bitvec-100
    bitvec-111
    bitvec-110
    bitvec-101))

;;; Non-declarative behavior
;;; compare with declareo-3 -- just swapped order of goals
;;; An error would be friendlier
(test "declareo-4"
  (run* (q)
    (s/chas-poso q)
    (s/declareo q))
  '())
|#

(test "lookupo-1"
  (run 3 (q)
    (fresh (x env val v)
      (s/declareo v)
      (s/chas-poso v)
      (symbolo x)
      (== `(num ,v) val)
      (== (list x env val) q)
      (lookupo x env val)))
  '(((_.0 ((_.0 . (num bitvec-100)) . _.1) (num bitvec-100)) (sym _.0))
    ((_.0 ((_.0 . (num bitvec-111)) . _.1) (num bitvec-111)) (sym _.0))
    ((_.0 ((_.0 . (num bitvec-110)) . _.1) (num bitvec-110)) (sym _.0))))

(test "lookupo-2"
  (run* (q)
    (fresh (x env val v1 v2)
      (s/declareo v1)
      (s/chas-poso v1)
      (s/declareo v2)
      (s/chas-nego v2)
      (== `((x . (num ,v1)) (y . (num ,v2))) env)
      (== (list env val) q)
      (lookupo 'x env val)))
  '((((x . (num bitvec-100)) (y . (num bitvec-001))) (num bitvec-100))
    (((x . (num bitvec-111)) (y . (num bitvec-111))) (num bitvec-111))
    (((x . (num bitvec-111)) (y . (num bitvec-101))) (num bitvec-111))
    (((x . (num bitvec-111)) (y . (num bitvec-001))) (num bitvec-111))
    (((x . (num bitvec-111)) (y . (num bitvec-011))) (num bitvec-111))
    (((x . (num bitvec-110)) (y . (num bitvec-101))) (num bitvec-110))
    (((x . (num bitvec-110)) (y . (num bitvec-111))) (num bitvec-110))
    (((x . (num bitvec-110)) (y . (num bitvec-001))) (num bitvec-110))
    (((x . (num bitvec-110)) (y . (num bitvec-011))) (num bitvec-110))
    (((x . (num bitvec-101)) (y . (num bitvec-111))) (num bitvec-101))
    (((x . (num bitvec-101)) (y . (num bitvec-011))) (num bitvec-101))
    (((x . (num bitvec-100)) (y . (num bitvec-011))) (num bitvec-100))
    (((x . (num bitvec-100)) (y . (num bitvec-111))) (num bitvec-100))
    (((x . (num bitvec-101)) (y . (num bitvec-101))) (num bitvec-101))
    (((x . (num bitvec-100)) (y . (num bitvec-101))) (num bitvec-100))
    (((x . (num bitvec-101)) (y . (num bitvec-001))) (num bitvec-101))))

(test "lookupo-3"
  (run* (q)
    (fresh (x env val v1 v2)
      (s/declareo v1)
      (s/chas-poso v1)
      (s/declareo v2)
      (s/chas-nego v2)      
      (== `((x . (num ,v1)) (y . (num ,v2))) env)
      (== (list env val) q)
      (lookupo 'y env val)))
  '((((x . (num bitvec-100)) (y . (num bitvec-001))) (num bitvec-001))
    (((x . (num bitvec-111)) (y . (num bitvec-111))) (num bitvec-111))
    (((x . (num bitvec-111)) (y . (num bitvec-101))) (num bitvec-101))
    (((x . (num bitvec-111)) (y . (num bitvec-001))) (num bitvec-001))
    (((x . (num bitvec-111)) (y . (num bitvec-011))) (num bitvec-011))
    (((x . (num bitvec-110)) (y . (num bitvec-101))) (num bitvec-101))
    (((x . (num bitvec-110)) (y . (num bitvec-111))) (num bitvec-111))
    (((x . (num bitvec-110)) (y . (num bitvec-001))) (num bitvec-001))
    (((x . (num bitvec-110)) (y . (num bitvec-011))) (num bitvec-011))
    (((x . (num bitvec-101)) (y . (num bitvec-111))) (num bitvec-111))
    (((x . (num bitvec-101)) (y . (num bitvec-011))) (num bitvec-011))
    (((x . (num bitvec-100)) (y . (num bitvec-011))) (num bitvec-011))
    (((x . (num bitvec-100)) (y . (num bitvec-111))) (num bitvec-111))
    (((x . (num bitvec-101)) (y . (num bitvec-101))) (num bitvec-101))
    (((x . (num bitvec-100)) (y . (num bitvec-101))) (num bitvec-101))
    (((x . (num bitvec-101)) (y . (num bitvec-001))) (num bitvec-001))))

(test "evalo-0a"
  (run* (q)
    (fresh (expr val)
      (== '3 expr)
      (== (list expr val) q)
      (evalo expr '() val)))
  '((3 (num bitvec-100))))

(test "evalo-0b"
  (run 10 (q)
    (fresh (expr val)
      (numbero expr)
      (== (list expr val) q)
      (evalo expr '() val)))
  '((0 (num bitvec-010))
    (-1 (num bitvec-001))
    (1 (num bitvec-100))
    (2 (num bitvec-100))
    (-2 (num bitvec-001))
    (3 (num bitvec-100))
    (4 (num bitvec-100))
    (5 (num bitvec-100))
    (-3 (num bitvec-001))
    (6 (num bitvec-100))))

(test "evalo-1"
  (run 10 (q)
    (fresh (expr val)
      (== (list expr val) q)
      (evalo expr '() val)))
  '((0 (num bitvec-010))
    (-1 (num bitvec-001))
    (1 (num bitvec-100))
    (2 (num bitvec-100))
    (-2 (num bitvec-001))
    (3 (num bitvec-100))
    (4 (num bitvec-100))
    (5 (num bitvec-100))
    (-3 (num bitvec-001))
    (6 (num bitvec-100))))

(test "evalo-2a"
  (run 1 (q)
    (fresh (expr val)
      (== `(+ 3 4) expr)
      (== (list expr val) q)
      (evalo expr '() val)))
  '(((+ 3 4) (num bitvec-100))))

(test "evalo-2b"
  (run* (q)
    (fresh (expr val)
      (== `(+ 3 4) expr)
      (== (list expr val) q)
      (evalo expr '() val)))
  '(((+ 3 4) (num bitvec-100))))

(test "evalo-2c"
  (run 1 (q)
    (fresh (expr val e1 e2)
      (== `(+ ,e1 ,e2) expr)
      (== (list expr val) q)
      (evalo expr '() val)))
  '(((+ -1 1) (num bitvec-111))))

(test "evalo-2d"
  (run 10 (q)
    (fresh (expr val e1 e2)
      (== `(+ ,e1 ,e2) expr)
      (== (list expr val) q)
      (evalo expr '() val)))
  '(((+ -1 1) (num bitvec-111))
    ((+ 1 -1) (num bitvec-111))
    ((+ 1 1) (num bitvec-100))
    ((+ 0 2) (num bitvec-100))
    ((+ -2 0) (num bitvec-001))
    ((+ 2 2) (num bitvec-100))
    ((+ -2 -2) (num bitvec-001))
    ((+ -3 -3) (num bitvec-001))
    ((+ 0 3) (num bitvec-100))
    ((+ -4 -4) (num bitvec-001))))

(test "evalo-2"
  (run 30 (q)
    (fresh (expr val op e1 e2)
      (== `(,op ,e1 ,e2) expr)
      (== (list expr val) q)
      (evalo expr '() val)))
  '(((+ -1 1) (num bitvec-111))
    ((+ 1 -1) (num bitvec-111))
    ((+ 1 1) (num bitvec-100))
    ((+ 0 2) (num bitvec-100))
    ((+ -2 0) (num bitvec-001))
    ((+ 2 2) (num bitvec-100))
    ((+ -2 -2) (num bitvec-001))
    ((+ -3 -3) (num bitvec-001))
    ((+ 0 3) (num bitvec-100))
    ((+ -4 -4) (num bitvec-001))
    ((+ -5 -5) (num bitvec-001))
    ((+ -6 -6) (num bitvec-001))
    ((+ -7 -7) (num bitvec-001))
    ((+ 0 4) (num bitvec-100))
    ((+ -8 -8) (num bitvec-001))
    ((+ 2 1) (num bitvec-100))
    ((+ 1 3) (num bitvec-100))
    ((+ 1 4) (num bitvec-100))
    ((+ 1 5) (num bitvec-100))
    ((+ -9 -9) (num bitvec-001))
    ((+ 1 0) (num bitvec-100))
    ((+ 0 0) (num bitvec-010))
    ((+ -10 -6) (num bitvec-001))
    ((+ 0 5) (num bitvec-100))
    ((+ 0 6) (num bitvec-100))
    ((+ -10 -10) (num bitvec-001))
    ((+ 0 -11) (num bitvec-001))
    ((+ -11 -11) (num bitvec-001))
    ((+ -12 0) (num bitvec-001))
    ((+ -12 -12) (num bitvec-001))))

(test "evalo-3"
  (run 10 (q)
    (fresh (expr env val e)
      (== `(let . ,e) expr)
      (== (list expr env val) q)
      (evalo expr env val)))
  '((((let ([_.0 0]) 0) _.1 (num bitvec-010)) (sym _.0))
    (((let ([_.0 -1]) 0) _.1 (num bitvec-010)) (sym _.0))
    (((let ([_.0 0]) _.0) _.1 (num bitvec-010)) (sym _.0))
    (((let ([_.0 1]) -1) _.1 (num bitvec-001)) (sym _.0))
    (((let ([_.0 1]) 0) _.1 (num bitvec-010)) (sym _.0))
    (((let ([_.0 -1]) _.0) _.1 (num bitvec-001)) (sym _.0))
    (((let ([_.0 1]) 1) _.1 (num bitvec-100)) (sym _.0))
    (((let ([_.0 2]) 2) _.1 (num bitvec-100)) (sym _.0))
    (((let ([_.0 1]) _.0) _.1 (num bitvec-100)) (sym _.0))
    (((let ([_.0 3]) 1) _.1 (num bitvec-100)) (sym _.0))))

(test "evalo-4"
  (run 15 (q)
    (fresh (expr env val id e)      
      (== `(let ([,id ,e]) x) expr)
      (== (list expr env val) q)
      (evalo expr env val)))
  '(((let ([x 0]) x) _.0 (num bitvec-010))
    ((let ([x -1]) x) _.0 (num bitvec-001))
    ((let ([x 1]) x) _.0 (num bitvec-100))
    ((let ([x 2]) x) _.0 (num bitvec-100))
    ((let ([x -2]) x) _.0 (num bitvec-001))
    ((let ([x 3]) x) _.0 (num bitvec-100))
    ((let ([x 4]) x) _.0 (num bitvec-100))
    ((let ([x 5]) x) _.0 (num bitvec-100))
    ((let ([x -3]) x) _.0 (num bitvec-001))
    ((let ([x 6]) x) _.0 (num bitvec-100))
    (((let ([_.0 0]) x) ((x . _.1) . _.2) _.1)
     (=/= ((_.0 x)))
     (sym _.0))
    ((let ([x 7]) x) _.0 (num bitvec-100))
    ((let ([x 8]) x) _.0 (num bitvec-100))
    (((let ([_.0 -1]) x) ((x . _.1) . _.2) _.1)
     (=/= ((_.0 x)))
     (sym _.0))
    ((let ([x 9]) x) _.0 (num bitvec-100))))

(test "evalo-5"
  (run 10 (q)
    (fresh (expr e1 e2 e3 val)
      (== `(ifzero ,e1 ,e2 ,e3) expr)
      (== (list expr val) q)
      (evalo expr '() val)))
  '(((ifzero 0 0 _.0) (num bitvec-010))
    ((ifzero 0 -1 _.0) (num bitvec-001))
    ((ifzero 0 -2 _.0) (num bitvec-001))
    ((ifzero 0 1 _.0) (num bitvec-100))
    ((ifzero 0 -3 _.0) (num bitvec-001))
    ((ifzero 0 -4 _.0) (num bitvec-001))
    ((ifzero 0 -5 _.0) (num bitvec-001))
    ((ifzero 0 2 _.0) (num bitvec-100))
    ((ifzero 0 -6 _.0) (num bitvec-001))
    ((ifzero 0 -7 _.0) (num bitvec-001))))

(test "evalo-6"
  (run 10 (q)
    (fresh (expr e1 e2 e3 val)
      (== '5 e1)
      (== `(ifzero ,e1 ,e2 ,e3) expr)
      (== (list expr val) q)
      (evalo expr '() val)))
  '(((ifzero 5 _.0 0) (num bitvec-010))
    ((ifzero 5 _.0 -1) (num bitvec-001))
    ((ifzero 5 _.0 1) (num bitvec-100))
    ((ifzero 5 _.0 -2) (num bitvec-001))
    ((ifzero 5 _.0 2) (num bitvec-100))
    ((ifzero 5 _.0 -3) (num bitvec-001))
    ((ifzero 5 _.0 -4) (num bitvec-001))
    ((ifzero 5 _.0 -5) (num bitvec-001))
    ((ifzero 5 _.0 -6) (num bitvec-001))
    ((ifzero 5 _.0 -7) (num bitvec-001))))

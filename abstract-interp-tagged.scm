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
      
      [(numbero expr) (s/z3-alphao expr val)]
      
      [(fresh (e1 e2 v1 v2)
         (== `(+ ,e1 ,e2) expr)
         (s/declareo v1)
         (s/declareo v2)
         ;; is this the ideal ordering?
         (s/z3-plus-tableo v1 v2 val)
         (evalo e1 env v1)
         (evalo e2 env v2))]

      [(fresh (e1 e2 v1 v2)
         (== `(* ,e1 ,e2) expr)
         (s/declareo v1)
         (s/declareo v2)
         ;; is this the ideal ordering?
         (s/z3-times-tableo v1 v2 val)
         (evalo e1 env v1)
         (evalo e2 env v2))]

      ;;; hmmm---seems like there is a problem here:
      ;;; if we want to make zero? and if work separately,
      ;;; evalo may return a bit vector or a boolean.
      ;;; How would this work, given the need for s/declareo?
      [(fresh (e1 e2 e3 v1)
         (== `(ifzero ,e1 ,e2 ,e3) expr)
         (s/declareo v1)
         ;; is this the ideal ordering?
         (evalo e1 env v1)
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
         (s/declareo v)
         (== `((,x . ,v) . ,env) env^)
         (evalo e env v)
         (evalo body env^ val))]
      
      )))

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


(test "lookupo-1"
  (run 3 (q)
    (fresh (x env val)
      (s/declareo val)
      (s/chas-poso val)
      (symbolo x)
      (== (list x env val) q)
      (lookupo x env val)))
  '(((_.0 ((_.0 . bitvec-100) . _.1) bitvec-100) (sym _.0))
    ((_.0 ((_.0 . bitvec-111) . _.1) bitvec-111) (sym _.0))
    ((_.0 ((_.0 . bitvec-110) . _.1) bitvec-110) (sym _.0))))

(test "lookupo-2"
  (run* (q)
    (fresh (x env val v1 v2)
      (s/declareo v1)
      (s/chas-poso v1)
      (s/declareo v2)
      (s/chas-nego v2)
      (== `((x . ,v1) (y . ,v2)) env)
      (== (list env val) q)
      (lookupo 'x env val)))
  '((((x . bitvec-100) (y . bitvec-001)) bitvec-100)
    (((x . bitvec-111) (y . bitvec-111)) bitvec-111)
    (((x . bitvec-111) (y . bitvec-101)) bitvec-111)
    (((x . bitvec-111) (y . bitvec-001)) bitvec-111)
    (((x . bitvec-111) (y . bitvec-011)) bitvec-111)
    (((x . bitvec-110) (y . bitvec-101)) bitvec-110)
    (((x . bitvec-110) (y . bitvec-111)) bitvec-110)
    (((x . bitvec-110) (y . bitvec-001)) bitvec-110)
    (((x . bitvec-110) (y . bitvec-011)) bitvec-110)
    (((x . bitvec-101) (y . bitvec-111)) bitvec-101)
    (((x . bitvec-101) (y . bitvec-011)) bitvec-101)
    (((x . bitvec-100) (y . bitvec-011)) bitvec-100)
    (((x . bitvec-100) (y . bitvec-111)) bitvec-100)
    (((x . bitvec-101) (y . bitvec-101)) bitvec-101)
    (((x . bitvec-100) (y . bitvec-101)) bitvec-100)
    (((x . bitvec-101) (y . bitvec-001)) bitvec-101)))

(test "lookupo-3"
  (run* (q)
    (fresh (x env val v1 v2)
      (s/declareo v1)
      (s/chas-poso v1)
      (s/declareo v2)
      (s/chas-nego v2)      
      (== `((x . ,v1) (y . ,v2)) env)
      (== (list env val) q)
      (lookupo 'y env val)))
  '((((x . bitvec-100) (y . bitvec-001)) bitvec-001)
    (((x . bitvec-111) (y . bitvec-111)) bitvec-111)
    (((x . bitvec-111) (y . bitvec-101)) bitvec-101)
    (((x . bitvec-111) (y . bitvec-001)) bitvec-001)
    (((x . bitvec-111) (y . bitvec-011)) bitvec-011)
    (((x . bitvec-110) (y . bitvec-101)) bitvec-101)
    (((x . bitvec-110) (y . bitvec-111)) bitvec-111)
    (((x . bitvec-110) (y . bitvec-001)) bitvec-001)
    (((x . bitvec-110) (y . bitvec-011)) bitvec-011)
    (((x . bitvec-101) (y . bitvec-111)) bitvec-111)
    (((x . bitvec-101) (y . bitvec-011)) bitvec-011)
    (((x . bitvec-100) (y . bitvec-011)) bitvec-011)
    (((x . bitvec-100) (y . bitvec-111)) bitvec-111)
    (((x . bitvec-101) (y . bitvec-101)) bitvec-101)
    (((x . bitvec-100) (y . bitvec-101)) bitvec-101)
    (((x . bitvec-101) (y . bitvec-001)) bitvec-001)))


(test "evalo-0a"
  (run* (q)
    (fresh (expr val)
      (s/declareo val)
      (== '3 expr)
      (== (list expr val) q)
      (evalo expr '() val)))
  '((3 bitvec-100)))

(test "evalo-0b"
  (run 10 (q)
    (fresh (expr val)
      (s/declareo val)
      (numbero expr)
      (== (list expr val) q)
      (evalo expr '() val)))
  '((0 bitvec-010)
    (-1 bitvec-001)
    (1 bitvec-100)
    (2 bitvec-100)
    (-2 bitvec-001)
    (3 bitvec-100)
    (4 bitvec-100)
    (5 bitvec-100)
    (-3 bitvec-001)
    (6 bitvec-100)))

(test "evalo-1"
  (run 10 (q)
    (fresh (expr val)
      (s/declareo val)
      (== (list expr val) q)
      (evalo expr '() val)))
  '((0 bitvec-010)
    (-1 bitvec-001)
    (1 bitvec-100)
    (2 bitvec-100)
    (-2 bitvec-001)
    (3 bitvec-100)
    (4 bitvec-100)
    (5 bitvec-100)
    (-3 bitvec-001)
    (6 bitvec-100)))

(test "evalo-2"
  (run 30 (q)
    (fresh (expr val op e1 e2)
      (s/declareo val)
      (== `(,op ,e1 ,e2) expr)
      (== (list expr val) q)
      (evalo expr '() val)))
  '(((+ 0 -1) bitvec-001)
    ((+ 1 -1) bitvec-111)
    ((+ 1 -2) bitvec-111)
    ((+ 1 -3) bitvec-111)
    ((+ 0 1) bitvec-100)
    ((+ 2 -4) bitvec-111)
    ((+ 1 1) bitvec-100)
    ((+ 1 2) bitvec-100)
    ((+ 1 -4) bitvec-111)
    ((+ 1 -5) bitvec-111)
    ((+ 1 -6) bitvec-111)
    ((+ -1 -6) bitvec-001)
    ((+ 2 -7) bitvec-111)
    ((+ -2 3) bitvec-111)
    ((+ -3 -6) bitvec-001)
    ((* 0 0) bitvec-010)
    ((+ -2 -7) bitvec-001)
    ((+ -2 -8) bitvec-001)
    ((* -1 -1) bitvec-100)
    ((+ 3 -8) bitvec-111)
    ((+ -4 -6) bitvec-001)
    ((* 1 1) bitvec-100)
    ((+ -5 4) bitvec-111)
    ((+ -6 -7) bitvec-001)
    ((* 2 -2) bitvec-001)
    ((+ 0 0) bitvec-010)
    ((+ -7 -9) bitvec-001)
    ((* -2 -2) bitvec-100)
    ((+ 4 -9) bitvec-111)
    ((+ -8 5) bitvec-111)))

(test "evalo-3"
  (run 10 (q)
    (fresh (expr env val e)
      (s/declareo val)
      (== `(let . ,e) expr)
      (== (list expr env val) q)
      (evalo expr env val)))
  '((((let ([_.0 0]) 0) _.1 bitvec-010) (sym _.0))
    (((let ([_.0 -1]) 0) _.1 bitvec-010) (sym _.0))
    (((let ([_.0 1]) -1) _.1 bitvec-001) (sym _.0))
    (((let ([_.0 2]) 1) _.1 bitvec-100) (sym _.0))
    (((let ([_.0 2]) 0) _.1 bitvec-010) (sym _.0))
    (((let ([_.0 3]) 0) _.1 bitvec-010) (sym _.0))
    (((let ([_.0 4]) 0) _.1 bitvec-010) (sym _.0))
    (((let ([_.0 5]) 0) _.1 bitvec-010) (sym _.0))
    (((let ([_.0 6]) 0) _.1 bitvec-010) (sym _.0))
    (((let ([_.0 7]) 0) _.1 bitvec-010) (sym _.0))))

(test "evalo-4"
  (run 10 (q)
    (fresh (expr env val id e)
      (s/declareo val)
      (== `(let ([,id ,e]) x) expr)
      (== (list expr env val) q)
      (evalo expr env val)))
  '((((let ([_.0 0]) x) ((x . _.1) . _.2) _.1)
     (=/= ((_.0 x)))
     (sym _.0))
    (((let ([_.0 -1]) x) ((x . _.1) . _.2) _.1)
     (=/= ((_.0 x)))
     (sym _.0))
    (((let ([_.0 1]) x) ((x . _.1) . _.2) _.1)
     (=/= ((_.0 x)))
     (sym _.0))
    (((let ([_.0 2]) x) ((x . _.1) . _.2) _.1)
     (=/= ((_.0 x)))
     (sym _.0))
    (((let ([_.0 -2]) x) ((x . _.1) . _.2) _.1)
     (=/= ((_.0 x)))
     (sym _.0))
    (((let ([_.0 3]) x) ((x . _.1) . _.2) _.1)
     (=/= ((_.0 x)))
     (sym _.0))
    (((let ([_.0 4]) x) ((x . _.1) . _.2) _.1)
     (=/= ((_.0 x)))
     (sym _.0))
    (((let ([_.0 5]) x) ((x . _.1) . _.2) _.1)
     (=/= ((_.0 x)))
     (sym _.0))
    (((let ([_.0 0]) x) ((_.1 . _.2) (x . _.3) . _.4) _.3)
     (=/= ((_.0 x)) ((_.1 x)))
     (sym _.0 _.1))
    (((let ([_.0 -3]) x) ((x . _.1) . _.2) _.1)
     (=/= ((_.0 x)))
     (sym _.0))))

(test "evalo-5"
  (run 10 (q)
    (fresh (expr e1 e2 e3 val)
      (s/declareo val)
      (== `(ifzero ,e1 ,e2 ,e3) expr)
      (== (list expr val) q)
      (evalo expr '() val)))
  '(((ifzero 0 0 _.0) bitvec-010)
    ((ifzero 0 -1 _.0) bitvec-001)
    ((ifzero 0 -2 _.0) bitvec-001)
    ((ifzero 0 1 _.0) bitvec-100)
    ((ifzero 0 -3 _.0) bitvec-001)
    ((ifzero 0 -4 _.0) bitvec-001)
    ((ifzero 0 -5 _.0) bitvec-001)
    ((ifzero 0 2 _.0) bitvec-100)
    ((ifzero 0 -6 _.0) bitvec-001)
    ((ifzero 0 -7 _.0) bitvec-001)))

(test "evalo-6"
  (run 10 (q)
    (fresh (expr e1 e2 e3 val)
      (s/declareo val)
      (== '5 e1)
      (== `(ifzero ,e1 ,e2 ,e3) expr)
      (== (list expr val) q)
      (evalo expr '() val)))
  '(((ifzero 5 _.0 0) bitvec-010)
    ((ifzero 5 _.0 -1) bitvec-001)
    ((ifzero 5 _.0 1) bitvec-100)
    ((ifzero 5 _.0 -2) bitvec-001)
    ((ifzero 5 _.0 2) bitvec-100)
    ((ifzero 5 _.0 -3) bitvec-001)
    ((ifzero 5 _.0 -4) bitvec-001)
    ((ifzero 5 _.0 -5) bitvec-001)
    ((ifzero 5 _.0 -6) bitvec-001)
    ((ifzero 5 _.0 -7) bitvec-001)))

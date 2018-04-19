(load "mk.scm")
(load "z3-driver.scm")
(load "test-check.scm")

;;; Classic 24 math puzzle, as described at:
;;;
;;; https://www.mathsisfun.com/puzzles/24-from-8-8-3-3-solution.html
;;;
;;; and
;;;
;;; http://www.4nums.com/game/difficulties/

;;; This version of code is restricted to integer values, which means solutions like
;;;
;;; 8/(3-(8/3))
;;; = 8/(1/3)
;;; = 24
;;;
;;; do *not* work!

(define remove-one-elemento
  (lambda (x ls out)
    (fresh (a d)
      (== `(,a . ,d) ls)
      (conde
        ((== x a) (== d out))
        ((=/= x a)
         (fresh (res)
           (== `(,a . ,res) out)
           (remove-one-elemento x d res)))))))

(define puzzleo
  (lambda (expr num* val num*^)
    (conde    
      
      [(numbero expr) (== expr val) (remove-one-elemento expr num* num*^)]

      [(fresh (a1 a2 n1 n2 num*^^)
         (== `(+ ,a1 ,a2) expr)
         (z/assert `(= ,val (+ ,n1 ,n2)))
         (puzzleo a1 num* n1 num*^^)
         (puzzleo a2 num*^^ n2 num*^))]

      [(fresh (a1 a2 n1 n2 num*^^)
         (== `(- ,a1 ,a2) expr)
         (z/assert `(= ,val (- ,n1 ,n2)))
         (puzzleo a1 num* n1 num*^^)
         (puzzleo a2 num*^^ n2 num*^))]

      [(fresh (a1 a2 n1 n2 num*^^)
         (== `(* ,a1 ,a2) expr)
         (z/assert `(= ,val (* ,n1 ,n2)))
         (puzzleo a1 num* n1 num*^^)
         (puzzleo a2 num*^^ n2 num*^))]

      [(fresh (a1 a2 n1 n2 num*^^)
         (== `(/ ,a1 ,a2) expr)
         (z/assert `(not (= ,n2 0)))
         (z/assert `(= ,val (div ,n1 ,n2)))
         (puzzleo a1 num* n1 num*^^)
         (puzzleo a2 num*^^ n2 num*^))]
      
      )))

;;; 35 seconds on Will's lappy
(test "24-puzzle-a"
  (run 1 (e) (puzzleo e '(1 1 1 8) 24 '()))
  '((* 8 (+ 1 (+ 1 1)))))

;;; boring!!
(test "24-puzzle-b"
  (run 1 (q)
    (fresh (e num* n1 n2 n3 n4)
      (== (list e num*) q)
      (== `(,n1 ,n2 ,n3 ,n4) num*)
      (puzzleo e num* 24 '())))
  '(((+ 24 (+ 0 (+ 0 0))) (24 0 0 0))))

(test "24-puzzle-c"
  (run 20 (e)
    (fresh (num* n1 n2 n3 n4)
      (z/assert `(< 1 ,n1))
      (z/assert `(< 1 ,n2))
      (z/assert `(< 1 ,n3))
      (z/assert `(< 1 ,n4))
      (== `(,n1 ,n2 ,n3 ,n4) num*)
      (puzzleo e num* 24 '())))
  '((+ 18 (+ 2 (+ 2 2)))
    (+ 15 (+ 3 (+ 3 3)))
    (+ 12 (+ 4 (+ 4 4)))
    (+ 9 (+ 5 (+ 5 5)))
    (+ 13 (+ 4 (+ 3 4)))
    (+ 11 (+ 6 (+ 3 4)))
    (+ 10 (+ 7 (+ 3 4)))
    (+ 9 (+ 8 (+ 3 4)))
    (+ 12 (+ 5 (+ 3 4)))
    (+ 8 (+ 9 (+ 3 4)))
    (+ 7 (+ 10 (+ 3 4)))
    (+ 14 (+ 4 (+ 3 3)))
    (+ 13 (+ 5 (+ 3 3)))
    (+ 12 (+ 6 (+ 3 3)))
    (+ 6 (+ 11 (+ 3 4)))
    (+ 11 (+ 7 (+ 3 3)))
    (+ 10 (+ 8 (+ 3 3)))
    (+ 9 (+ 9 (+ 3 3)))
    (+ 8 (+ 10 (+ 3 3)))
    (+ 7 (+ 11 (+ 3 3)))))

(test "24-puzzle-d"
  (run 10 (e)
    (fresh (num* n1 n2 n3 n4 op1 op2 op3 e1 e2 e3 e4)
      (z/assert `(< 0 ,n1))
      (z/assert `(< 0 ,n2))
      (z/assert `(< 0 ,n3))
      (z/assert `(< 0 ,n4))
      (== `(,n1 ,n2 ,n3 ,n4) num*)
      (== `(,op1 (,op2 ,e1 ,e2) (,op3 ,e3 ,e4)) e)
      (puzzleo e num* 24 '())))
  '((+ (+ 21 1) (+ 1 1))
    (+ (+ 18 2) (+ 2 2))
    (+ (+ 15 3) (+ 3 3))  
    (+ (+ 12 4) (+ 4 4))
    (+ (+ 16 3) (+ 2 3))
    (+ (+ 14 5) (+ 2 3))
    (+ (+ 13 6) (+ 2 3))
    (+ (+ 12 7) (+ 2 3))
    (+ (+ 15 4) (+ 2 3))
    (+ (+ 11 8) (+ 2 3))))

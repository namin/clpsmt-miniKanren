(load "mk.scm")
(load "z3-driver.scm")
;;(load "cvc4-driver.scm")
(load "test-check.scm")
(load "interp-program-synthesizer-blog-post.scm")


;; Attempt to use miniKanren + SMT to solve synthesis problems described in the blog post:

;; Building a Program Synthesizer
;; James Bornholt
;; 10 July 2018
;;
;; https://homes.cs.washington.edu/~bornholt/post/building-synthesizer.html
;;
;; "Build a program synthesis tool, to generate programs from specifications, in 20 lines of code using Rosette."
;;
;; Code from the blog post is on GitHub?:
;;
;; https://gist.github.com/jamesbornholt/b51339fb8b348b53bfe8a5c66af66efe



;; Challenge 1: Find an integer whose absolute value is 5

;; Code from the blog post:

#|
#lang rosette/safe

; Compute the absolute value of `x`.
(define (absv x)
  (if (< x 0) (- x) x))

; Define a symbolic variable called y of type integer.
(define-symbolic y integer?)

; Solve a constraint saying |y| = 5.
(solve
  (assert (= (absv y) 5)))
|#

(test "challenge-1-a"
  (run* (y)
    (numbero y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (absv ',y))
           5))
  '(5 -5))

(test "challenge-1-b"
  (run* (y)
    (numbero y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (absv ,y))
           5))
  '(5 -5))

(test "challenge-1-c"
  (run* (y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (absv ',y))
           5))
  '(5 -5))

(test "challenge-1-d"
  (run 5 (y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (absv ,y))
           5))
  '(5 -5 '5 '-5 ((let ([_.0 _.1]) 5) (num _.1) (sym _.0))))



;; Challenge 2: Try to outsmart the solver by finding a 'y' whose
;; absolute value is less than 0.  There is no such 'y'.

#|
(solve (assert (< (absv y) 0)))
|#

(test "challenge-2-a"
  (run* (y)
    (numbero y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (< (absv ',y) 0))
           #t))
  '())

(test "challenge-2-b"
  (run* (y)
    (numbero y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (< (absv ,y) 0))
           #t))
  '())

(test "challenge-2-c"
  (run* (y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (< (absv ',y) 0))
           #t))
  '())

(test "challenge-2-d"
  (run* (y)
    (numbero y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (< (absv ',y) 1))
           #t))
  '(0))

(test "challenge-2-e"
  (run* (y)
    (numbero y)
    (evalo `(let ((absv (lambda (x)
                          (if (< x 0) (- x) x))))
              (< (absv ',y) 3))
           #t))
  '(0 2 1 -2 -1))


;; Challenge 3: Little interpreter for arithmetic

#|
(define (interpret p)
  (match p
    [(plus a b)  (+ (interpret a) (interpret b))]
    [(mul a b)   (* (interpret a) (interpret b))]
    [(square a)  (expt (interpret a) 2)]
    [_ p]))
|#

#|
(interpret (plus (square 7) 3)) => 52
|#

;; Using evalo:
(test "challenge-3-a"
  (run* (y)
    (numbero y)
    (evalo `(let ((plus (lambda (a b) (+ a b))))
              (let ((mul (lambda (a b) (* a b))))
                (let ((square (lambda (a) (* a a))))
                  (plus (square 7) 3))))
           y))
  '(52))

(test "challenge-3-b"
  (run* (y)
    (evalo `(let ((plus (lambda (a b) (+ a b))))
              (let ((mul (lambda (a b) (* a b))))
                (let ((square (lambda (a) (* a a))))
                  (plus (square 7) 3))))
           y))
  '(52))

(test "challenge-3-c"
  (run* (y)
    (numbero y)
    (evalo `(let ((square (lambda (a) (* a a))))
              (+ (square 7) 3))
           y))
  '(52))

(test "challenge-3-d"
  (run* (y)
    (evalo `(let ((square (lambda (a) (* a a))))
              (+ (square 7) 3))
           y))
  '(52))


;; Challenge 4: Find 'y' such that (square (plus y 2)) => 25

#|
(solve 
  (assert 
    (= (interpret (square (plus y 2))) 25)))
|#

(test "challenge-4-a"
  (run* (y)
    (numbero y)
    (evalo `(let ((plus (lambda (a b) (+ a b))))
              (let ((mul (lambda (a b) (* a b))))
                (let ((square (lambda (a) (* a a))))
                  (square (+ ',y 2)))))
           25))
  '(-7 3))

(test "challenge-4-b"
  (run* (y)
    (numbero y)
    (evalo `(let ((plus (lambda (a b) (+ a b))))
              (let ((mul (lambda (a b) (* a b))))
                (let ((square (lambda (a) (* a a))))
                  (square (+ ,y 2)))))
           25))
  '(-7 3))

(test "challenge-4-c"
  (run* (y)
    (evalo `(let ((plus (lambda (a b) (+ a b))))
              (let ((mul (lambda (a b) (* a b))))
                (let ((square (lambda (a) (* a a))))
                  (square (+ ',y 2)))))
           25))
  '(-7 3))

(test "challenge-4-d"
  (run* (y)
    (numbero y)
    (evalo `(let ((square (lambda (a) (* a a))))
              (square (+ ',y 2)))
           25))
  '(-7 3))

(test "challenge-4-e"
  (run* (y)
    (numbero y)
    (evalo `(let ((square (lambda (a) (* a a))))
              (square (+ ,y 2)))
           25))
  '(-7 3))

(test "challenge-4-f"
  (run* (y)
    (evalo `(let ((square (lambda (a) (* a a))))
              (square (+ ',y 2)))
           25))
  '(-7 3))







#!eof

(test "primitive-positive"
  (run* (q)
    (numbero q)
    (z/assert `(= (+ 2 5) ,q)))
  '(7))

(test "primitive-sub-pos"
  (run* (q)
    (numbero q)
    (z/assert `(= (- 8 3) ,q)))
  '(5))

(test "primitive-negative"
  (run* (q)
    (numbero q)
    (z/assert `(= (- 2 5) ,q)))
  '(-3))

(test "positive-evalo"
  (run* (q) (evalo `(+ 2 5) q))
  '(7))

(test "sub-evalo-pos"
  (run* (q) (evalo `(- 8 3) q))
  '(5))

(test "negative-evalo"
  (run* (q) (evalo `(- 2 5) q))
  '(-3))

#!eof

(test "evalo-simple-let-a"
  (run* (q)
    (evalo '(let ((foo (+ 1 2))) (* foo foo)) q))
  '(9))

(test "evalo-symbolic-execution-a"
  (run 1 (q)
    (fresh (alpha beta gamma)
      (== (list alpha beta gamma) q)
      (evalo `(let ((a ',alpha))
                (let ((b ',beta))
                  (let ((c ',gamma))
                    (let ((x (if (!= a 0)
                                 -2
                                 0)))
                      (let ((y (if (and (< b 5) (= a 0) (!= c 0))
                                   1
                                   0)))
                        (let ((z (if (< b 5)
                                     2
                                     0)))
                          (if (!= (+ x (+ y z)) 3)
                              'good
                              'bad)))))))
             'bad)))  
  '((0 4 1)))

(test "many-1"
  (run* (q)
    (fresh (x y)
      (evalo `(+ (* ',x ',y) (* ',x ',y)) 6)
      (== q (list x y))))
  '((3 1) (1 3) (-1 -3) (-3 -1)))

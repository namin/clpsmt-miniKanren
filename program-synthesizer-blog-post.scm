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


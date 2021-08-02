#lang racket/base

(require racket/list
         racket/system
	 racket/include
         (rename-in racket/base (eval eval1))
         (rename-in racket/base (open-output-file open-output-file1))
         (rename-in racket/system (system system1)))

(provide run run*
         == =/=
         fresh
         conde
         symbolo numbero
         absento
         succeed
         fail
         project
         z/assert
         z/
         do-defer-smt-checks!
         get-next-model?
         toggle-get-next-model?!
         z3-counter-check-sat
         z3-counter-get-model)

;; extra stuff for racket
(define ns (make-base-namespace))

(define (eval e)
  (eval1 e ns))

(define (open-output-file path options)
  (open-output-file1 path #:exists options))

(define (system command)
  (if (system1 command) 0 -1))

(include "z3-driver.scm")

;; extra stuff for racket
;; due mostly to samth
(define (list-sort f l) (sort l f))

(define (remp f l) (filter-not f l))

(define (call-with-string-output-port f)
  (define p (open-output-string))
  (f p)
  (get-output-string p))

(define (exists f l) (ormap f l))

(include "mk.scm")
(define (do-defer-smt-checks!)
  (set! defer-smt-checks #t))
(define (toggle-get-next-model?!)
  (set! get-next-model? (not get-next-model?)))

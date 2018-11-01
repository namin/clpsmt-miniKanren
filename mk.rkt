#lang racket/base

(require racket/list
         racket/system
         racket/include)

(provide run run*
         == =/=
         fresh
         conde
         symbolo numbero
         absento
         z/assert
         z/)

;; extra stuff for racket
;; due mostly to samth
(define (list-sort f l) (sort l f))

(define (remp f l) (filter-not f l))

(define (call-with-string-output-port f)
  (define p (open-output-string))
  (f p)
  (get-output-string p))

(define (exists f l) (ormap f l))

(include "z3-driver.rkt")
(include "mk.scm")

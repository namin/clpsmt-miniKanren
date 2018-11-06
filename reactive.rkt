#lang racket

(require "mk.rkt")
(require graphics/graphics)

(provide (all-from-out "mk.rkt")
         (all-defined-out))

(define pi 3.14159)
(define two-pi (* pi 2))

;;; window size
(define horiz 300)
(define vert 300)

(define horiz-center (quotient horiz 2))
(define vert-center (quotient vert 2))


(define (current-milliseconds)
  (inexact->exact (floor (current-inexact-milliseconds))))

(define (clp-bouncing-ball)
  (open-graphics)
  (let ((w (open-viewport "clp-bouncing-ball" horiz vert)))
    (dynamic-wind
        void
        (let ((ball-size 5.0)
              (left-wall-x 0)
              (top-wall-y 0)
              (right-wall-x 295)
              (bottom-wall-y 295)
              (start-time 0))
          (let loop ((old-time start-time)
                     (old-ball* `((0 0 2 3) ; x y x-velocity y-velocity
                                  (7 2 4 1)
                                  (100 100 -1 -2))))
            (sleep 0.05)
            (let ((new-time (add1 old-time)))
              (let ((ans (list new-time new-time)))
                (let ((factor-ls ans))
                  (for-each (lambda (ball)
                              (match ball
                                [`(,x ,y ,x-vel ,y-vel)
                                 ((clear-solid-ellipse w)
                                  (make-posn x y)
                                  ball-size
                                  ball-size)]))
                            old-ball*)
                      
                  (let ((new-ball* (map (lambda (ball)
                                          (match ball
                                            [`(,x ,y ,x-vel ,y-vel)
                                             (let ((possible-new-x (+ x x-vel))
                                                   (possible-new-y (+ y y-vel)))
                                               (let ((x-vel (if (and
                                                                 (>= possible-new-x left-wall-x)
                                                                 (<= possible-new-x right-wall-x))
                                                                x-vel
                                                                (- x-vel)))
                                                     (y-vel (if (and
                                                                 (>= possible-new-y top-wall-y)
                                                                 (<= possible-new-y bottom-wall-y))
                                                                y-vel
                                                                (- y-vel))))
                                                 (let ((new-x (+ x x-vel))
                                                       (new-y (+ y y-vel)))
                                                   ((draw-solid-ellipse w)
                                                    (make-posn new-x new-y)
                                                    ball-size
                                                    ball-size)
                                                       
                                                   (let ((new-ball (list new-x new-y x-vel y-vel)))
                                                     new-ball))))]))
                                        old-ball*)))
                    (loop new-time new-ball*)))))))
        (begin
          (close-viewport w)
          (close-graphics)))))

(define (draw-bouncing-ball-by-time)
  (open-graphics)
  (let ((w (open-viewport "draw-bouncing-ball-by-time" horiz vert)))
    (dynamic-wind
        void
        (let ((ball-size 5.0)
              (left-wall-x 0)
              (top-wall-y 0)
              (right-wall-x 295)
              (bottom-wall-y 295)
              (start-time (modulo (quotient (current-milliseconds) 50) 50)))
          (let loop ((old-time start-time)
                     (old-ball* `((0 0 2 3) ; x y x-velocity y-velocity
                                  (7 2 4 1)
                                  (100 100 -1 -1))))
            (let ((new-time (modulo (quotient (current-milliseconds) 50) 50)))
              (if (= new-time old-time)
                  (loop old-time old-ball*)
                  (let ((ans (list new-time new-time)))
                    (let ((factor-ls ans))
                      (for-each (lambda (ball)
                                  (match ball
                                    [`(,x ,y ,x-vel ,y-vel)
                                     ((clear-solid-ellipse w)
                                      (make-posn x y)
                                      ball-size
                                      ball-size)]))
                                old-ball*)
                      
                      (let ((new-ball* (map (lambda (ball)
                                              (match ball
                                                [`(,x ,y ,x-vel ,y-vel)
                                                 (let ((possible-new-x (+ x x-vel))
                                                       (possible-new-y (+ y y-vel)))
                                                   (let ((x-vel (if (and
                                                                     (>= possible-new-x left-wall-x)
                                                                     (<= possible-new-x right-wall-x))
                                                                    x-vel
                                                                    (- x-vel)))
                                                         (y-vel (if (and
                                                                     (>= possible-new-y top-wall-y)
                                                                     (<= possible-new-y bottom-wall-y))
                                                                    y-vel
                                                                    (- y-vel))))
                                                     (let ((new-x (+ x x-vel))
                                                           (new-y (+ y y-vel)))
                                                       ((draw-solid-ellipse w)
                                                        (make-posn new-x new-y)
                                                        ball-size
                                                        ball-size)
                                                       
                                                       (let ((new-ball (list new-x new-y x-vel y-vel)))
                                                         new-ball))))]))
                                           old-ball*)))
                        (loop new-time new-ball*))))))))
        (begin
          (close-viewport w)
          (close-graphics)))))

(define (old-draw-bouncing-ball-by-time)
  (open-graphics)
  (let ((w (open-viewport "old-draw-bouncing-ball-by-time" horiz vert)))
    (dynamic-wind
        void
        (let ((ball-size 5.0)              
              (start-time (modulo (quotient (current-milliseconds) 50) 50)))
          (let loop ((old-time start-time)
                     (old-pos (make-posn 0 0))
                     (x-velocity 2)
                     (y-velocity 3))
            (let ((new-time (modulo (quotient (current-milliseconds) 50) 50)))
              (if (= new-time old-time)
                  (loop old-time old-pos x-velocity y-velocity)
                  (let ((ans (list new-time new-time)))
                    (let ((factor-ls ans))
                      ((clear-solid-ellipse w)
                       old-pos
                       ball-size
                       ball-size)
                      
                      (let ((possible-new-x (+ (posn-x old-pos) x-velocity)))
                        (let ((x-velocity (if (<= possible-new-x 50)
                                              x-velocity
                                              (- x-velocity))))
                          (let ((new-x (+ (posn-x old-pos) x-velocity)))
                            (let ((new-pos (make-posn new-x
                                                      (+ (posn-y old-pos) y-velocity))))
                              ((draw-solid-ellipse w)
                               new-pos
                               ball-size
                               ball-size)
                              
                              (loop new-time new-pos x-velocity y-velocity)))))))))))
        (begin
          (close-viewport w)
          (close-graphics)))))

;;; Draw lines, whose x/y coordinates reflect all the factors of n,
;;; where n is the current time in seconds, mod 50.
;;; Uses busy-waiting to wait for the second to increment.
(define (draw-line-mult-by-time)
  (open-graphics)
  (let ((w (open-viewport "draw-line-mult-by-time" horiz vert)))
    (dynamic-wind
        void
        (let loop ((old-time (modulo (current-seconds) 50)))
          (let ((time (modulo (current-seconds) 50)))
            (if (= time old-time)
                (loop time)
                (let ((ans (run* (q)
                             (fresh (a b)
                               (== (list a b) q)
                               (z/assert `(and (>= ,a 0) (>= ,b 0)))
                               (z/assert `(and (<= ,a 60) (<= ,b 60)))
                               (z/assert `(= (* ,a ,b) ,time))))))
                  (let ((factor-ls ans))
                    (displayln factor-ls)
                    ((clear-viewport w))
                    ((draw-string w) (make-posn horiz-center vert-center) (number->string time))
                    (for-each
                      (lambda (x/y)
                        ((draw-line w)
                         (make-posn 0 0)
                         (make-posn (+ 0 (* (car x/y) 10))
                                    (+ 0 (* (cadr x/y) 10)))))
                      factor-ls)
                    (loop time))))))
        (begin
          (close-viewport w)
          (close-graphics)))))

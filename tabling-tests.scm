(load "mk.scm")
;(load "z3-driver.scm")
(load "test-check.scm")

;; some tests inspired by
;; https://github.com/webyrd/tabling

(test "path-tabled"
  (letrec ((arc (lambda (x y)
                  (conde
                    ((== x 'a) (== y 'b))
                    ((== x 'b) (== y 'a))
                    ((== x 'b) (== y 'd)))))
           (path (tabled
                     (lambda (x y)
                       (conde
                         ((arc x y))
                         ((fresh (z)
                            (arc x z)
                            (path z y))))))))
    (run* (q)
      (path 'a q)))
  '(b a d))

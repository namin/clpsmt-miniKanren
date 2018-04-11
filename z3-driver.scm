(define read-sat
  (lambda (fn)
    (let ([p (open-input-file "out.txt")])
      (let ([r (read p)])
        (close-input-port p)
        (eq? r 'sat)))))

(define check-sat
  (lambda (xs)
    (let ([p (open-output-file "out.smt" 'replace)])
      (for-each (lambda (x) (fprintf p "~a\n" x))
                (append xs '((check-sat) (exit))))
      (close-output-port p)
      (system "z3 out.smt >out.txt")
      (read-sat "out.txt"))))

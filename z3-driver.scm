(define read-sat
  (lambda (fn)
    (let ([p (open-input-file fn)])
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

(define read-model
  (lambda (fn)
    (let ([p (open-input-file fn)])
      (let ([r (read p)])
        (if (eq? r 'sat)
            (let ([m (read p)])
              (close-input-port p)
              (map (lambda (x)
                     (cons (cadr x) (cadddr (cdr x))))
                   (cdr m)))
            (begin
              (close-input-port p)
              #f))))))

(define get-model
  (lambda (xs)
    (let ([p (open-output-file "out.smt" 'replace)])
      (for-each (lambda (x) (fprintf p "~a\n" x))
                (append xs '((check-sat) (get-model) (exit))))
      (close-output-port p)
      (system "z3 out.smt >out.txt")
      (read-model "out.txt"))))

(define check-model-unique
  (lambda (xs model)
    (let ([r
           (check-sat
            (append xs
                    (list
                     (cons
                      'assert
                      (list
                       (cons
                        'or
                        (map
                         (lambda (xv)
                           `(not (= ,(car xv) ,(cdr xv))))
                         model)))))))])
      (not r))))

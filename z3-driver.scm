(define z3-counter-check-sat 0)
(define z3-counter-get-model 0)

(define log-all-calls #f)

(define read-sat
  (lambda (fn)
    (let ([p (open-input-file fn)])
      (let ([r (read p)])
        (close-input-port p)
	(when (eq? r 'unknown)
	  (printf "unknown\n"))
        (eq? r 'sat)))))

(define call-z3
  (lambda (xs)
    (let ([p (open-output-file "out.smt" 'replace)])
      (for-each (lambda (x) (fprintf p "~a\n" x)) xs)
      (close-output-port p)
      ;; WEB -- I think this is equivalent to, but faster than, the commented three calls to sed, below
      ;; see https://unix.stackexchange.com/questions/97428/sed-how-to-do-several-consecutive-substitutions-but-process-file-only-once#97437
      (system "perl -i -pe 's/#t/true/g; s/#f/false/g; s/bitvec-/#b/g' out.smt")
      ;; (system "perl -i -pe 's/#t/true/g' out.smt")
      ;; (system "perl -i -pe 's/#f/false/g' out.smt")
      ;; (system "perl -i -pe 's/bitvec-/#b/g' out.smt")
      
      (let ((r (system "z3 out.smt >out.txt")))
        (when log-all-calls
          (system (format "cp out.smt out~d.smt" (+ z3-counter-check-sat z3-counter-get-model)))
          (system (format "cp out.txt out~d.txt" (+ z3-counter-check-sat z3-counter-get-model))))
        (system "perl -i -pe 's/#b/bitvec-/g' out.txt")
        (when (not (= r 0))
          (error 'call-z3 "error in z3 out.smt > out.txt"))))))

(define check-sat
  (lambda (xs)
    (call-z3 (append xs '((check-sat) (exit))))
    (set! z3-counter-check-sat (+ z3-counter-check-sat 1))
    (read-sat "out.txt")))

(define read-model
  (lambda (fn)
    (let ([p (open-input-file fn)])
      (let ([r (read p)])
        (if (eq? r 'sat)
            (let ([m (read p)])
              (close-input-port p)
              (map (lambda (x)
                     (cons (cadr x)
                           (if (null? (caddr x))
                               (let ([r (cadddr (cdr x))])
                                 (cond
                                   ((eq? r 'false) #f)
                                   ((eq? r 'true) #t)
                                   ((and (pair? (cadddr x)) (eq? (cadr (cadddr x)) 'BitVec)) r)
                                   (else (eval r))))
                               `(lambda ,(map car (caddr x)) ,(cadddr (cdr x))))))
                   (cdr m)))
            (begin
              (close-input-port p)
              #f))))))

(define get-model
  (lambda (xs)
    (call-z3 (append xs '((check-sat) (get-model) (exit))))
    (set! z3-counter-get-model (+ z3-counter-get-model 1))
    (read-model "out.txt")))

(define neg-model
  (lambda (model)
    (cons
     'assert
     (list
      (cons
       'or
       (map
        (lambda (xv)
          `(not (= ,(car xv) ,(cdr xv))))
        model))))))

(define check-model-unique
  (lambda (xs model)
    (let ([r
           (check-sat
            (append xs (list (neg-model model))))])
      (not r))))

(define get-all-models
  (lambda (xs ms)
    (let* ([ys (append xs (map neg-model ms))])
      (if (not (check-sat ys))
          (reverse ms)
          (get-all-models xs (cons (get-model ys) ms))))))

(define get-next-model
  (lambda (xs ms)
    (let* ([ms (map (lambda (m)
                      (filter (lambda (x) ; ignoring functions
                                (or (number? (cdr x))
                                    (symbol? (cdr x)) ; for bitvectors
                                    (boolean? (cdr x)) ; for booleans
                                    )) m))
                    ms)])
      (if (member '() ms) #f  ; if we're skipping a model, let us stop
          (let ([ys (append xs (map neg-model ms))])
            (and (check-sat ys)
                 (get-model ys)))))))

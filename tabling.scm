;; starting point is miniKanren tabling code by Ramana Kumar
;; taken from webyrd/tabling

(define make-cache (lambda (ansv*) (vector 'cache ansv*)))
(define cache-ansv* (lambda (v) (vector-ref v 1)))
(define cache-ansv*-set! (lambda (v val) (vector-set! v 1 val)))

(define make-ss (lambda (cache ansv* f) (vector 'ss cache ansv* f)))
(define ss? (lambda (v) (and (vector? v) (eq? (vector-ref v 0) 'ss))))
(define ss-cache (lambda (v) (vector-ref v 1)))
(define ss-ansv* (lambda (v) (vector-ref v 2)))
(define ss-f (lambda (v) (vector-ref v 3)))

(define subunify (lambda (arg ans s) (subsumed ans arg s)))

(define subsumed
  (lambda (arg ans s)
    (let ((arg (walk arg s))
          (ans (walk ans s)))
      (cond
        ((eq? arg ans) s)
        ((var? ans) (ext-s-no-check ans arg s))
        ((and (pair? arg) (pair? ans))
         (let ((s (subsumed (car arg) (car ans) s)))
           (and s (subsumed (cdr arg) (cdr ans) s))))
        ((equal? arg ans) s)
        (else #f)))))

(define reuse
  (lambda (argv cache s)
    (let fix ((start (cache-ansv* cache)) (end '()))
      (let loop ((ansv* start))
        (if (eq? ansv* end)
            (list (make-ss cache start (lambdaf@ () (fix (cache-ansv* cache) start))))
            (choice (subunify argv (reify-var (car ansv*) s) s)
                    (lambdaf@ () (loop (cdr ansv*)))))))))

(define master
  (lambda (argv cache)
    (lambdag@ (s)
      (and
        (for-all
         (lambda (ansv) (not (subsumed argv ansv s)))
         (cache-ansv* cache))
        (begin
          (cache-ansv*-set! cache
                            (cons (reify-var argv s)
                                  (cache-ansv* cache)))
          s)))))

(define-syntax tabled
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (let ((table '()))
       (lambda (x ...)
         (let ((argv (list x ...)))
           (lambdag@ (s)
             (let ((key (reify argv s)))
               (cond
                 ((assoc key table)
                  => (lambda (key.cache) (reuse argv (cdr key.cache) s)))
                 (else (let ((cache (make-cache '())))
                         (set! table (cons `(,key . ,cache) table))                         
                         ((fresh () g g* ... (master argv cache)) s))))))))))))


(define ss-ready? (lambda (ss) (not (eq? (cache-ansv* (ss-cache ss)) (ss-ansv* ss)))))
(define w? (lambda (w) (and (pair? w) (ss? (car w)))))

(define w-check
  (lambda (w sk fk)
    (let loop ((w w) (a '()))
      (cond
        ((null? w) (fk))
        ((ss-ready? (car w))
         (sk (lambdaf@ ()
               (let ((f (ss-f (car w)))
                     (w (append (reverse a) (cdr w))))
                 (if (null? w) (f)
                     (mplus (f) (lambdaf@ () w)))))))
        (else (loop (cdr w) (cons (car w) a)))))))

(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((w) ew) ((a^) e2) ((a f) e3))
     (let ((a-inf e))
       (cond
         ((not a-inf) e0)
         ((procedure? a-inf) (let ((f^ a-inf)) e1))
         ((and (pair? a-inf) (procedure? (cdr a-inf)))
          (let ((a (car a-inf)) (f (cdr a-inf))) e3))
         ((w? a-inf) (w-check a-inf
                              (lambda (f^) e1)
                              (lambda () (let ((w a-inf)) ew))))           
         (else (let ((a^ a-inf)) e2)))))))

(define take
  (lambda (n f)
    (if (and n (zero? n)) 
      '()
      (case-inf (f)
        (() '())
        ((f) (take n f))
        ((w) '())
        ((a) a)
        ((a f) (cons (car a) (take (and n (- n 1)) f)))))))

(define bind
  (lambda (a-inf g)
    (case-inf a-inf
      (() (mzero))
      ((f) (inc (bind (f) g)))
      ((w) (map (lambda (ss)
                  (make-ss (ss-cache ss) (ss-ansv* ss)
                           (lambdaf@ () (bind ((ss-f ss)) g))))
                w))
      ((a) (g a))
      ((a f) (mplus (g a) (lambdaf@ () (bind (f) g)))))))

(define mplus
  (lambda (a-inf f)
    (case-inf a-inf
      (() (f))
      ((f^) (inc (mplus (f) f^)))
      ((w) (lambdaf@ () (let ((a-inf (f)))
                          (if (w? a-inf)
                              (append a-inf w)
                              (mplus a-inf (lambdaf@ () w))))))
      ((a) (choice a f))
      ((a f^) (choice a (lambdaf@ () (mplus (f) f^)))))))

(define reify-v
  (lambda (n)
    (var n)))

(define make-reify
  (lambda (rep)
    (lambda (v s)
      (let ((v (walk* v s)))
        (walk* v (reify-s rep v empty-s))))))

(define reify-s
  (lambda (rep v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (ext-s-no-check v (rep (length s)) s))
        ((pair? v) (reify-s rep (cdr v) (reify-s rep (car v) s)))
        (else s)))))

(define reify (make-reify reify-name))
(define reify-var (make-reify reify-v))

;;; Do not include in diss
(define-syntax run+
  (syntax-rules ()
    ((_ (x) g0 g* ...)
     (take+
      (lambdaf@ ()
        ((fresh (x) g0 g* ... 
                (lambdag@ (a)
                  (cons (reify x a) '())))
         empty-s))))))

(define take+
  (lambda (f)
    (case-inf (f)
      (() '())
      ((f) (take+ f))
      ((w) '())
      ((a) (cons (car a) (lambda () '())))
      ((a f) (cons (car a) (lambda () (take+ f)))))))

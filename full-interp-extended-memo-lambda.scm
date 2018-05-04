;; Add memo-lambda form, and pass around a list of memo tables (which themselves are association lists) as an argument
;;
;; match is currently commented out, since threading through the table appears tedious and error prone

;; The definition of 'letrec' is based based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.

(define (evalo expr val)
  (fresh (tables-out)
    (eval-expo expr initial-env initial-tables tables-out val)))

(define (eval-expo expr env tables-in tables-out val)
  (conde
    ((== `(quote ,val) expr)
     (== tables-in tables-out)
     (absento 'closure val)
     (absento 'prim val)
     (not-in-envo 'quote env))

    ((numbero expr)
     (== tables-in tables-out)
     (== expr val))

    ((symbolo expr)
     (== tables-in tables-out)
     (lookupo expr env val))

    ((fresh (x body)
       (== `(lambda ,x ,body) expr)
       (== `(closure (lambda ,x ,body) ,env) val)
       (== tables-in tables-out)
       (conde
         ;; Variadic
         ((symbolo x))
         ;; Multi-argument
         ((list-of-symbolso x)))
       (not-in-envo 'lambda env)))

    ((fresh (x body name)
       ;; should memo-lambda take a user-defined name?
       ;; if not, how to lookup the right table for the resulting closure?
       (== `(memo-lambda ,name ,x ,body) expr)
       (== `(closure (memo-lambda ,name ,x ,body) ,env) val)
       ;; add a new, empty memo table for the new memo'd closure
       (== `((,name . ()) . ,tables-in) tables-out)
       (conde
         ;; Variadic
         ((symbolo x))
         ;; Multi-argument
         ((list-of-symbolso x)))
       (not-in-envo 'memo-lambda env)))

    ((fresh (x e body a env^ tables^)
       (== `(let ((,x ,e)) ,body) expr)
       (symbolo x)
       (ext-envo x a env env^)
       (eval-expo e env tables-in tables^ a)
       (eval-expo body env^ tables^ tables-out val)))
    
    ((fresh (rator x rands body env^ a* res tables^ tables^^)
       (== `(,rator . ,rands) expr)
       ;; variadic
       (symbolo x)
       (== `((,x . (val . ,a*)) . ,env^) res)
       (eval-expo rator env tables-in tables^ `(closure (lambda ,x ,body) ,env^))
       (eval-expo body res tables^ tables^^ val)
       (eval-listo rands env tables^^ tables-out a*)))

    ((fresh (rator x* rands body env^ a* res tables^ tables^^)
       (== `(,rator . ,rands) expr)
       ;; Multi-argument
       (eval-expo rator env tables-in tables^ `(closure (lambda ,x* ,body) ,env^))
       (eval-listo rands env tables^ tables^^ a*)
       (ext-env*o x* a* env^ res)
       (eval-expo body res tables^^ tables-out val)))

    ((fresh (rator x* rands body env^ a* tables^ tables^^ name)
       (== `(,rator . ,rands) expr)
       ;; Multi-argument memo-lambda closure
       (eval-expo rator env tables-in tables^ `(closure (memo-lambda ,name ,x* ,body) ,env^))
       (eval-listo rands env tables^ tables^^ a*)

       (fresh (table entry)
         ;; look up the memo table in 'tables^^' corresponding to 'name'.
         ;; there should be a table, since 'memo-lambda' adds an association of the name mapped to the empty table
         (find-tableo tables^^ name table)
         
         ;; look up the arguments to the memo'd closure, 'a*', in the resulting table:         
         (table-lookupo a* table entry)

         (conde
           ((== 'in-progress entry)
            ;; fail, since the recursive function has been called
            ;; again with the same arguments, before the original call
            ;; has finished (divergent computation)

            ;; fail!
            (== #f #t))
           ((== `(memo-value ,val) entry)
            ;;   if 'a*' is associated with '(memo-value val)', then the value of the call is 'val'
            )
           ((== 'no-entry entry)

            ;;   if 'a*' does not yet have an entry in the table,
            (fresh (res in-progress-table table^ final-value-table new-tables new-tables^)

              ;;   associate 'a*' with 'in-progress' in an extended table, and update a new tables to use the "in-progress" table
              (== `((,a* in-progress) . ,table) in-progress-table)
              (== `((,name . ,in-progress-table) . ,tables^^) new-tables)

              ;;   let the result of evaluating the body of the memo-closure be 'val', as usual  (this is just the normal evaluation of a procedure call)
              (ext-env*o x* a* env^ res)
              (eval-expo body res new-tables new-tables^ val)

              ;;   look up the memo table for name,
              (find-tableo new-tables^ name table^)
              
              ;;   associate 'a*' with '(memo-value val)',
              (== `((,a* (memo-value ,val)) . ,table^) final-value-table)
              
              ;;   unify the updated table with 'tables-out'
              (== `((,name . ,final-value-table) . ,new-tables^) tables-out)
              
              ))))))

    ((fresh (rator x* rands a* prim-id tables^ tables^^)
       (== `(,rator . ,rands) expr)
       (eval-expo rator env tables-in tables^ `(prim . ,prim-id))
       (eval-primo prim-id a* val)
       (eval-listo rands env tables^ tables-out a*)))

    #|
    ((handle-matcho expr env tables-in tables-out val))
    |#

    ((fresh (p-name x body letrec-body)
       ;; single-function variadic letrec version
       (== `(letrec ((,p-name (lambda ,x ,body)))
              ,letrec-body)
           expr)
       (conde
         ; Variadic
         ((symbolo x))
         ; Multiple argument
         ((list-of-symbolso x)))
       (not-in-envo 'letrec env)
       (eval-expo letrec-body
                  `((,p-name . (rec . (lambda ,x ,body))) . ,env)
                  tables-in
                  tables-out
                  val)))
    
    ((prim-expo expr env tables-in tables-out val))
    
    ))

(define empty-env '())

(define (lookupo x env t)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (conde
         ((== `(val . ,t) b))
         ((fresh (lam-expr)
            (== `(rec . ,lam-expr) b)
            (== `(closure ,lam-expr ,env) t)))))
      ((=/= x y)
       (lookupo x rest t)))))

(define (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))

(define (find-tableo tables table-name table)
  (fresh (tn t rest)
    (== `((,tn . ,t) . ,rest) tables)
    (conde
      ((== tn table-name)
       (== t table))
      ((=/= tn table-name)
       (find-tableo rest table-name table)))))

(define (table-lookupo x table entry)
  (conde
    ((== '() table) (== 'no-entry entry))
    ((fresh (y v rest)
       (== `((,y ,v) . ,rest) table)
       (conde
         ((== x y)
          (== v entry))
         ((=/= x y)
          (table-lookupo x rest entry)))))))

(define (eval-listo expr env tables-in tables-out val)
  (conde
    ((== '() expr)
     (== tables-in tables-out)
     (== '() val))
    ((fresh (a d v-a v-d tables^)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expo a env tables-in tables^ v-a)
       (eval-listo d env tables^ tables-out v-d)))))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(define (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(define (ext-envo x a env out)
  (fresh ()
    (== `((,x . (val . ,a)) . ,env) out)
    (symbolo x)))

(define (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((,x . (val . ,a)) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

(define (eval-primo prim-id a* val)
  (conde
    [(== prim-id 'cons)
     (fresh (a d)
       (== `(,a ,d) a*)
       (== `(,a . ,d) val))]
    [(== prim-id 'car)
     (fresh (d)
       (== `((,val . ,d)) a*)
       (=/= 'closure val))]
    [(== prim-id 'cdr)
     (fresh (a)
       (== `((,a . ,val)) a*)
       (=/= 'closure a))]
    [(== prim-id 'not)
     (fresh (b)
       (== `(,b) a*)
       (conde
         ((=/= #f b) (== #f val))
         ((== #f b) (== #t val))))]
    [(== prim-id 'equal?)
     (fresh (v1 v2)
       (== `(,v1 ,v2) a*)
       (conde
         ((== v1 v2) (== #t val))
         ((=/= v1 v2) (== #f val))))]
    ;; FIXME (webyrd) -- symbol?, and perhaps other type predicates, doesn't handle booleans (fails)
    [(== prim-id 'symbol?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((symbolo v) (== #t val))
         ((numbero v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)))))]
    [(== prim-id 'null?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((== '() v) (== #t val))
         ((=/= '() v) (== #f val))))]
    [(conde
       [(== prim-id '+)]
       [(== prim-id '-)]
       [(== prim-id '*)]
       [(== prim-id '/)])
     (fresh (a1 a2)
       (== `(,a1 ,a2) a*)
       ;; we could use list-of-numbero instead
       ;; but it causes more divergence with run*
       ;; (list-of-numbero a*)
       (numbero a1)
       (numbero a2)
       (z/assert `(= ,val (,prim-id ,a1 ,a2))))]
    [(== prim-id '!=)
     (fresh (a1 a2)
       (== `(,a1 ,a2) a*)
       (numbero a1)
       (numbero a2)
       (conde
         [(z/assert `(not (= ,a1 ,a2))) (== #t val)]
         [(z/assert `(= ,a1 ,a2)) (== #f val)]))]
    [(conde
       [(== prim-id '=)]
       [(== prim-id '>)]
       [(== prim-id '>=)]
       [(== prim-id '<)]
       [(== prim-id '=<)])
     (fresh (a1 a2)
       (== `(,a1 ,a2) a*)
       ;; we could use list-of-numbero instead
       ;; but it causes more divergence with run*
       ;; (list-of-numbero a*)
       (numbero a1)
       (numbero a2)
       (conde
         [(z/assert `(,prim-id ,a1 ,a2)) (== #t val)]
         [(z/assert `(not (,prim-id ,a1 ,a2))) (== #f val)]))]
    ))

(define (prim-expo expr env tables-in tables-out val)
  (conde
    ((boolean-primo expr env tables-in tables-out val))
    ((and-primo expr env tables-in tables-out val))
    ((or-primo expr env tables-in tables-out val))
    ((if-primo expr env tables-in tables-out val))))

(define (boolean-primo expr env tables-in tables-out val)
  (fresh ()
    (== tables-in tables-out)
    (conde
      ((== #t expr) (== #t val))
      ((== #f expr) (== #f val)))))

(define (and-primo expr env tables-in tables-out val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (not-in-envo 'and env)
    (ando e* env tables-in tables-out val)))

(define (ando e* env tables-in tables-out val)
  (conde
    ((== '() e*) (== #t val) (== tables-in tables-out))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env tables-in tables-out val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((== #f v)
          (== #f val)
          (eval-expo e1 env tables-in tables-out v))
         ((=/= #f v)
          (fresh (tables^)
            (eval-expo e1 env tables-in tables^ v)
            (ando `(,e2 . ,e-rest) env tables^ tables-out val))))))))

(define (or-primo expr env tables-in tables-out val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (not-in-envo 'or env)
    (oro e* env tables-in tables-out val)))

(define (oro e* env tables-in tables-out val)
  (conde
    ((== '() e*) (== #f val) (== tables-in tables-out))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env tables-in tables-out val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((=/= #f v)
          (== v val)
          (eval-expo e1 env tables-in tables-out v))
         ((== #f v)
          (fresh (tables^)
            (eval-expo e1 env tables-in tables^ v)
            (oro `(,e2 . ,e-rest) env tables^ tables-out val))))))))

(define (if-primo expr env tables-in tables-out val)
  (fresh (e1 e2 e3 t tables^)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    (eval-expo e1 env tables-in tables^ t)
    (conde
      ((=/= #f t) (eval-expo e2 env tables^ tables-out val))
      ((== #f t) (eval-expo e3 env tables^ tables-out val)))))

(define initial-env `((list . (val . (closure (lambda x x) ,empty-env)))
                      (not . (val . (prim . not)))
                      (equal? . (val . (prim . equal?)))
                      (symbol? . (val . (prim . symbol?)))
                      (cons . (val . (prim . cons)))
                      (null? . (val . (prim . null?)))
                      (car . (val . (prim . car)))
                      (cdr . (val . (prim . cdr)))
                      (+ . (val . (prim . +)))
                      (- . (val . (prim . -)))
                      (* . (val . (prim . *)))
                      (/ . (val . (prim . /)))
                      (= . (val . (prim . =)))
                      (!= . (val . (prim . !=)))
                      (> . (val . (prim . >)))
                      (>= . (val . (prim . >=)))
                      (< . (val . (prim . <)))
                      (=< . (val . (prim . =<)))
                      . ,empty-env))

(define initial-tables '())

#|
(define handle-matcho
  (lambda  (expr env tables-in tables-out val tables^)
    (fresh (against-expr mval clause clauses)
      (== `(match ,against-expr ,clause . ,clauses) expr)
      (not-in-envo 'match env)
      (eval-expo against-expr env tables-in tables^ mval)
      (match-clauses mval `(,clause . ,clauses) env tables^ tables-out val))))
|#

(define (not-symbolo t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (not-numbero t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((symbolo t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(define (self-eval-literalo t)
  (conde
    ((numbero t))
    ((booleano t))))

(define (literalo t)
  (conde
    ((numbero t))
    ((symbolo t) (=/= 'closure t))
    ((booleano t))
    ((== '() t))))

(define (booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

#|
(define (regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

(define (match-clauses mval clauses env tables-in tables-out val)
  (fresh (p result-expr d penv)
    (== `((,p ,result-expr) . ,d) clauses)
    (conde
      ((fresh (env^)
         (p-match p mval '() penv)
         (regular-env-appendo penv env env^)
         (eval-expo result-expr env^ tables-in tables-out val)))
      ((p-no-match p mval '() penv)
       (match-clauses mval d env val)))))

(define (var-p-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= 'closure mval)
    (conde
      ((== mval val)
       (== penv penv-out)
       (lookupo var penv val))
      ((== `((,var . (val . ,mval)) . ,penv) penv-out)
       (not-in-envo var penv)))))

(define (var-p-no-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= mval val)
    (== penv penv-out)
    (lookupo var penv val)))

(define (p-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (== p mval)
     (== penv penv-out))
    ((var-p-match p mval penv penv-out))
    ((fresh (var pred val)
      (== `(? ,pred ,var) p)
      (conde
        ((== 'symbol? pred)
         (symbolo mval))
        ((== 'number? pred)
         (numbero mval)))
      (var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-match quasi-p mval penv penv-out)))))

(define (p-no-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (=/= p mval)
     (== penv penv-out))
    ((var-p-no-match p mval penv penv-out))
    ((fresh (var pred val)
       (== `(? ,pred ,var) p)
       (== penv penv-out)
       (symbolo var)
       (conde
         ((== 'symbol? pred)
          (conde
            ((not-symbolo mval))
            ((symbolo mval)
             (var-p-no-match var mval penv penv-out))))
         ((== 'number? pred)
          (conde
            ((not-numbero mval))
            ((numbero mval)
             (var-p-no-match var mval penv penv-out)))))))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-no-match quasi-p mval penv penv-out)))))

(define (quasi-p-match quasi-p mval penv penv-out)
  (conde
    ((== quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
      (== (list 'unquote p) quasi-p)
      (p-match p mval penv penv-out)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (== `(,v1 . ,v2) mval)
       (=/= 'unquote a)
       (quasi-p-match a v1 penv penv^)
       (quasi-p-match d v2 penv^ penv-out)))))

(define (quasi-p-no-match quasi-p mval penv penv-out)
  (conde
    ((=/= quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (=/= 'closure mval)
       (p-no-match p mval penv penv-out)))
    ((fresh (a d)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== penv penv-out)
       (literalo mval)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== `(,v1 . ,v2) mval)
       (conde
         ((quasi-p-no-match a v1 penv penv^))
         ((quasi-p-match a v1 penv penv^)
          (quasi-p-no-match d v2 penv^ penv-out)))))))
|#

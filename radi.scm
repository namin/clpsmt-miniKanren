;; A terminating 0cfa abstract interpreter via ADI caching
;; based on work by David Darais
;; For ADI, see http://david.darais.com/assets/papers/abstracting-definitional-interpreters/adi.pdf

(define (conso x xs r)
  (== (cons x xs) r))

(define (mapo r xs f)
  (conde
   [(== xs '()) (== r '())]
   [(fresh [y ys z zs]
      (conso y ys xs)
      (conso z zs r)
      (f y z)
      (mapo zs ys f))]))

(define (foldlo f x xs r)
  (conde
   [(== xs '()) (== x r)]
   [(fresh [y ys z]
      (conso y ys xs)
      (f x y z)
      (foldlo f z ys r))]))

(define (keyso s ks)
  (conde
   [(== s '()) (== ks '())]
   [(fresh [kv r k v kr]
      (conso kv r s)
      (== kv `(,k ,v))
      (conso k kr ks)
      (keyso r kr))]))

(define (maxo x y z)
  (conde
    [(z/assert `(<= ,x ,y)) (== y z)]
    [(z/assert `(> ,x ,y)) (== x z)]))

(define (alloco s n)
  (fresh [ks r]
    (keyso s ks)
    (foldlo maxo 0 ks r)
    (z/assert `(= (+ ,r 1) ,n))))

(define (lookupo x bs out)
  (conde
   [(== bs '()) (== out #f)]
   [(fresh [b br y v]
      (conso b br bs)
      (== b `(,y ,v))
      (conde
       [(== y x) (== v out)]
       [(=/= y x) (lookupo x br out)]))]))

(define (ino x xs)
  (fresh [y ys]
    (conso y ys xs)
    (conde
     [(== y x)]
     [(=/= y x)
      (ino x ys)])))

(define (not-ino x xs)
  (conde
   [(== xs '())]
   [(fresh [y ys]
      (conso y ys xs)
      (=/= y x)
      (not-ino x ys))]))

(define (includo a b)
  (conde
   [(== a '())]
   [(fresh [a1 ar]
      (conso a1 ar a)
      (ino a1 b)
      (includo ar b))]))

(define (set-equivo a b)
  (fresh ()
    (includo a b)
    (includo b a)))

(define (excludo a b)
  (fresh [a1 ar b]
    (conde
     [(not-ino a1 b)]
     [(excludo ar b)])))

(define (not-set-equivo a b)
  (conde
   [(excludo a b)]
   [(excludo b a)]))

(define (set-uniono a b c)
  (conde
   [(== a '()) (== b c)]
   [(fresh [xa ra]
      (conso xa ra a)
      (conde
       [(ino xa b)
        (set-uniono ra b c)]
       [(not-ino xa b)
        (fresh [rc]
          (conso xa rc c)
          (set-uniono ra b rc))]))]))

(define (set-unionso xs out)
  (foldlo set-uniono
          '()
          xs
          out))

(define (set-unionso2 xs out)
  (foldlo (lambda (a b c)
            (set-uniono a `(,b) c))
          '()
          xs
          out))

(define (injo i a)
  (conde
    [(z/assert `(< ,i 0)) (== a 'neg)]
    [(== i 0) (== a 'zer)]
    [(z/assert `(> ,i 0)) (== a 'pos)]))

(define (combo f u os1 s1 s2 s3)
  (conde
   [(== s2 '())
    (== s3 '())]
   [(== s1 '())
    (fresh [a2 r2]
      (conso a2 r2 s2)
      (combo f u os1 os1 r2 s3))]
   [(fresh [a1 r1 a2 r2 sa sm]
      (conso a1 r1 s1)
      (conso a2 r2 s2)
      (f a1 a2 sa)
      (combo f u os1 r1 s2 sm)
      (u sa sm s3))]))

(define (combino f s1 s2 s3)
  (combo f set-uniono s1 s1 s2 s3))

(define (plusdo a b s)
  (conde
    [(== a 'neg) (== b 'neg) (== s `(neg))]
    [(== a 'neg) (== b 'zer) (== s `(neg))]
    [(== a 'neg) (== b 'pos) (== s `(neg zer pos))]
    [(== a 'zer) (== b 'neg) (== s `(neg))]
    [(== a 'zer) (== b 'zer) (== s `(zer))]
    [(== a 'zer) (== b 'pos) (== s `(pos))]
    [(== a 'pos) (== b 'neg) (== s `(neg zer pos))]
    [(== a 'pos) (== b 'zer) (== s `(pos))]
    [(== a 'pos) (== b 'pos) (== s `(pos))]))

(define (plusso s1 s2 s3)
  (combino plusdo s1 s2 s3))

(define (timeso a b c)
  (conde
   [(== a 'zer) (== c 'zer)]
   [(== b 'zer) (== c 'zer)]
   [(== a 'neg) (== b 'neg) (== c 'pos)]
   [(== a 'neg) (== b 'pos) (== c 'neg)]
   [(== a 'pos) (== b 'neg) (== c 'neg)]
   [(== a 'pos) (== b 'pos) (== c 'pos)]))

(define (timesdo a b s)
  (fresh [c]
    (timeso a b c)
    (== s [c])))

(define (timesso s1 s2 s3)
  (combino timesdo s1 s2 s3))

(define (lubo a1 a2 a3)
  (fresh [is1 cs1 is2 cs2 is3 cs3]
    (== `(aval ,is1 ,cs1) a1)
    (== `(aval ,is2 ,cs2) a2)
    (== `(aval ,is3 ,cs3) a3)
    (set-uniono is1 is2 is3)
    (set-uniono cs1 cs2 cs3)))

(define (adivalo e s ocache icache out)
  (conde
   [(fresh [x l]
      (== `(var ,x) e)
      (lookupo x s l)
      (conde
       [(== l #f) (== out '())]
       [(=/= l #f) (== out `((,l ,s ,icache)))]))]
   [(fresh [i a]
      (== `(int ,i) e)
      (injo i a)
      (== out `(((aval (,a) ()) ,s ,ocache))))]
   [(fresh [e1 e2 s1 s2]
      (== `(plus ,e1 ,e2) e)
      (fresh [s1 r]
        (adivalpto e1 s ocache icache s1)
        (mapo r s1
              (lambda (a1 o)
                (fresh [oi is1 cs1 sp icachep s2]
                  (== a1 `((aval ,is1 ,cs1) ,sp ,icachep))
                  (adivalpto e2 sp ocache icachep s2)
                  (mapo oi s2
                        (lambda (a2 o2)
                          (fresh [is2 cs2 sp2 icachep2 is3]
                            (== a2 `((aval ,is2 ,cs2) ,sp2 ,icachep2))
                            (plusso is1 is2 is3)
                            (== o2 `((aval ,is3 ()) ,sp2 ,icachep2)))))
                  (set-unionso oi o))))
        (== r out)))]
   [(fresh [e1 e2 s1 s2]
      (== `(times ,e1 ,e2) e))]
   [(fresh [x y e0]
      (== `(lam ,x ,y ,e) e)
      (== out `(((aval () ((,x ,y ,e0)))) ,s ,icache)))]
   [(fresh [e1 e2]
      (== `(app ,e1 ,e2) e))]
   [(fresh [e1 e2 e3]
      (== `(if0 ,e1 ,e2 ,e3) e))]))

 (define (adivalwo e s ocache icache out)
  (fresh [res r c r-out c-out]
    (adivalo e s ocache icache res)
    (mapo r res (lambda (a o)
                  (fresh [x y z]
                    (== a `(,x ,y ,z))
                    (== o `(,x ,y)))))
    (mapo c res (lambda (a o)
                  (fresh [x y z]
                    (== a `(,x ,y ,z))
                    (== o z))))
    (set-unionso2 r r-out)
    (set-unionso c c-out)
    (== out `(,r-out ,c-out))))

(define (adivalpo e s ocache icache out)
  (fresh [r]
    (lookupo `(,e ,s) icache r)
    (conde
      [(=/= r #f) (== out `(,r ,icache))]
      [(== r #f)
       (fresh [r0 o icachep r icachepp icacheppp]
         (lookupo `(,e ,s) ocache o)
         (conde
           [(== o #f) (== r0 '())]
           [(=/= o #f) (== r0 o)])
         (conso `((,e ,s) ,r0) icache icachep)
         (adivalwo e s ocache icachep `(,r ,icachepp))
         (set-uniono `(((,e ,s) ,r)) icachepp icacheppp)
         (== out `(,r ,icacheppp)))])))

(define (adivalpto e s ocache icache out)
  (fresh [r icachep]
    (adivalpo e s ocache icache `(,r ,icachep))
    (mapo out r (lambda (a o)
                  (fresh [vs sp]
                    (== a `(,vs ,sp))
                    (== o `(,vs ,sp ,icachep)))))))

(define (lfppo e cache out)
  (fresh [r cachep]
    (adivalpo e '() cache '() `(,r ,cachep))
    (conde
     [(set-equivo cache cachep) (== out cache)]
     [(not-set-equivo cache cachep) (lfppo e cachep out)])))

(define (lfpo e out)
  (lfppo e '() out))

(define (analyzeo e out)
  (fresh [cache]
    (lfpo e cache)
    (lookupo `(,e ()) cache out)))

(define (iterpo n e cache)
  (conde
   [(== n 0) cache]
   [(fresh [r cachep n-1]
      (z/assert `(= (+ 1 ,n-1) ,n))
      (adivalpo e '() cache '() `(,r ,cachep))
      (iterpo n-1 e cachep))]))

(define (itero n e out)
  (iterpo n e '() out))

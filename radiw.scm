(define (map-singletono k v m)
  (== `((,k ,v)) m))

(define (ino x xs)
  (fresh [y ys]
    (conso y ys xs)
    (conde
      [(== y x)
       (not-ino x ys)]
      [(=/= y x)
       (ino x ys)])))

(define (not-ino x xs)
  (conde
    [(== xs '())]
    [(fresh [y ys]
       (conso y ys xs)
       (=/= y x)
       (not-ino x ys))]))

(define (is-ino a s b)
  (conde
    [(ino a s) (== b #t)]
    [(not-ino a s) (== b #f)]))

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

(define (lookupo x bs out)
  (conde
    ;; #f is never bound as a value in the store,
    ;; we don't have booleans.
    [(== bs '()) (== out #f)]
    [(fresh [b br y v]
       (conso b br bs)
       (== `(,y ,v) b)
       (conde
         [(== y x) (== v out)]
         [(=/= y x) (lookupo x br out)]))]))

(define (boto v)
  (== `(aval () ()) v))

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
  (fresh [a1 ar]
    (conso a1 ar a)
    (conde
      [(not-ino a1 b)]
      [(ino a1 b) (excludo ar b)])))

(define (not-set-equivo a b)
  (conde
    [(excludo a b)]
    [(includo a b) (excludo b a)]))

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

(define (uo a b c)
  (fresh [ai bi ci aj bj cj]
    (== a `(aval ,ai ,aj))
    (== b `(aval ,bi ,bj))
    (== c `(aval ,ci ,cj))
    (set-uniono ai bi ci)
    (set-uniono aj bj cj)))

(define (joinso xs o)
  (fresh [b]
    (boto b)
    (foldlo uo
            b
            xs
            o)))

(define (replaceo x vo bs out)
  (fresh [b br y v]
    (conso b br bs)
    (== b `(,y ,v))
    (conde
      [(== y x) (conso `(,x ,vo) br out)]
      [(=/= y x)
       (fresh [r-out]
         (conso b r-out out)
         (replaceo x vo br r-out))])))

(define (add-uo x v s out)
  (fresh [l]
    (lookupo x s l)
    (conde
      [(== l #f) (== out `((,x ,v) . ,s))]
      [(=/= l #f)
       (fresh [vo]
         (uo v l vo)
         (replaceo x vo s out))])))

(define (add-uo1 m xv r)
  (fresh [x v]
    (== `(,x ,v) xv)
    (add-uo x v m r)))

(define (muo m1 m2 mo)
  (foldlo add-uo1
          m1
          m2
          mo))

(define (mjoinso ms o)
  (foldlo muo
          '()
          ms
          o))

(define (injo i a)
  (conde
    [(== -1 i) (== a 'neg)]
    [(== i 0) (== a 'zer)]
    [(== 1 i) (== a 'pos)]))

(define (into i v)
  (fresh [a]
    (injo i a)
    (== v `(aval (,a) ()))))

(define (combo f u os1 s1 s2 s3)
  (conde
    [(== '() s2)
     (== '() s3)]
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
    [(=/= a 'zer) (== b 'zer) (== c 'zer)]
    [(== a 'neg) (== b 'neg) (== c 'pos)]
    [(== a 'neg) (== b 'pos) (== c 'neg)]
    [(== a 'pos) (== b 'neg) (== c 'neg)]
    [(== a 'pos) (== b 'pos) (== c 'pos)]))

(define (timesdo a b s)
  (fresh [c]
    (timeso a b c)
    (== s `(,c))))

(define (timesso s1 s2 s3)
  (combino timesdo s1 s2 s3))

(define (cloo x y body out)
  (== out `(aval () ((,x ,y ,body)))))

(define (unzip3o rs as bs cs)
  (conde
    [(== rs '())
     (== as '())
     (== bs '())
     (== cs '())]
    [(fresh [r1 rr a1 ar b1 br c1 cr]
       (conso r1 rr rs)
       (conso a1 ar as)
       (conso b1 br bs)
       (conso c1 cr cs)
       (== r1 `(,a1 ,b1 ,c1))
       (unzip3o rr ar br cr))]))

(define (adivalo e s oc ic v so co)
  (conde
    [(fresh [x l]
       (== `(var ,x) e)
       (lookupo x s l)
       (== s so)
       (== ic co)
       (conde
         [(== l #f) (boto v)]
         [(=/= l #f) (== l v)]))]
    [(fresh [i a]
       (== `(int ,i) e)
       (into i v)
       (== s so)
       (== ic co))]
    [(fresh [e1 e2 v1 v2 n1 n2 s1 s2 n so1 co1 so2 co2]
       (== `(plus ,e1 ,e2) e)
       (== v1 `(aval ,n1 ,s1))
       (== v2 `(aval ,n2 ,s2))
       (adivalpo e1 s oc ic v1 so1 co1)
       (adivalpo e2 s oc ic v2 so2 co2)
       (plusso n1 n2 n)
       (== v `(aval ,n ()))
       (muo so1 so2 so)
       (muo co1 co2 co))]
    [(fresh [e1 e2 v1 v2 n1 n2 s1 s2 n so1 co1 so2 co2]
       (== `(times ,e1 ,e2) e)
       (== v1 `(aval ,n1 ,s1))
       (== v2 `(aval ,n2 ,s2))
       (adivalpo e1 s oc ic v1 so1 co1)
       (adivalpo e2 s oc ic v2 so2 co2)
       (timesso n1 n2 n)
       (== v `(aval ,n ()))
       (muo so1 so2 so)
       (muo co1 co2 co))]
    [(fresh [x y body]
       (== `(lam ,x ,y ,body) e)
       (cloo x y e v)
       (== s so)
       (== ic co))]
    [(fresh [e1 e2 is fs v1 s1 c1 v2 s2 c2 r vs ss3 cs3 v3 s3 c3]
       (== `(app ,e1 ,e2) e)
       (adivalpo e1 s oc ic v1 s1 c1)
       (== `(aval ,is ,fs) v1)
       (adivalpo e2 s oc ic v2 s2 c2)
       (mapo r fs (lambda (i z)
                    (fresh [x y body c sa1 sa vo so co]
                      (== i `(,x ,y ,body))
                      (== z `(,vo ,so ,co))
                      (cloo x y body c)
                      (add-uo x c s sa1)
                      (add-uo y v2 sa1 sa)
                      (adivalpo body sa oc ic vo so co))))
       (unzip3o r vs ss3 cs3)
       (joinso vs v3)
       (mjoinso ss3 s3)
       (mjoinso cs3 c3))]
    [(fresh [e1 e2 e3 r2 r3p r3n r3 v1 n1 b1 s1 c1 v2 s2 c2 v3 s3 c3 s23 c23]
       (== `(if0 ,e1 ,e2 ,e3) e)
       (adivalpo e1 s oc ic v1 s1 c1)
       (== v1 `(aval ,n1 ,b1))
       (is-ino 'zer n1 r2)
       (is-ino 'pos n1 r3p)
       (is-ino 'neg n1 r3n)
       (conde
         [(== r2 #t) (adivalpo e2 s oc ic v2 s2 c2)]
         [(== r2 #f) (bot v2) (== s s2) (== ic c2)])
       (conde
         [(== r3p #t) (== r3 #t)]
         [(== r3p #f) (== r3n #t) (== r3 #t)]
         [(== r3p #f) (== r3n #f) (== r3 #f)])
       (conde
         [(== r3 #t) (adivalpo e3 s oc ic v3 s3 c3)]
         [(== r3 #f) (bot v3) (== s s3) (== ic c3)])
       (uo v2 v3 v)
       (muo s2 s3 s23)
       (muo s1 s23 so)
       (muo c3 c3 c23)
       (muo c1 c23 co))]))

(define (adivalpo e s oc ic v so co)
  (fresh [r]
    (lookupo e ic r)
    (conde
      [(=/= r #f)
       (== v r)
       (== s so)
       (== ic co)]
      [(== r #f)
       (fresh [r0 v0 ic1 s1 ic2]
         (lookupo e oc r0)
         (conde
           [(== r0 #f) (boto v0)]
           [(=/= r0 #f) (== v0 r0)])
         (add-uo e v0 ic ic1)
         (adivalo e s oc ic1 v so ic2)
         (add-uo e v ic2 co))])))

(define (lfppo e s c so co)
  (fresh [v sp cp]
    (adivalpo e s c '() v sp cp)
    (cond
      [(== `(,sp ,cp) `(,s ,c))
       (== so s)
       (== co c)]
      [(=/= `(,sp ,cp) `(,s ,c))
       (lfppo e sp cp so co)])))

(define (lfpo e so co)
  (lfppo e '() '() so co))

(define (analyzeo e vo)
  (fresh [so co]
    (lfpo e so co)
    (lookupo e co vo)))

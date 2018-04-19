(load "mk.scm")
(load "z3-driver.scm")
(load "test-check.scm")
(load "full-interp.scm")

(test "symbolic-execution-1a"
  (run 10 (q)
    (evalo
      `((lambda (n)
          (if (= 137 n)
              'foo
              'bar))
        ,q)
      'foo))
  '(137
    '137
    (((lambda _.0 137)) (sym _.0))
    (((lambda _.0 137) _.1) (num _.1) (sym _.0))
    (((lambda _.0 137) _.1 _.2) (num _.1 _.2) (sym _.0))
    (((lambda _.0 137) _.1 _.2 _.3) (num _.1 _.2 _.3) (sym _.0))
    (((lambda _.0 137) list) (sym _.0))
    (((lambda _.0 137) list _.1) (num _.1) (sym _.0))
    ((match _.0 (_.0 137) . _.1) (num _.0))
    (((lambda _.0 137) _.1 _.2 _.3 _.4) (num _.1 _.2 _.3 _.4) (sym _.0))))

(test "symbolic-execution-1b"
  (run* (q)
    (evalo
      `((lambda (n)
          (if (= 137 n)
              'foo
              'bar))
        ',q)
      'foo))
  '(137))

(test "symbolic-execution-2a"
  (run 10 (q)
    (evalo
      `((lambda (n)
          (if (= 137 n)
              'foo
              'bar))
        ,q)
      'bar))
  '(138 139 140 141 142 143 144 145 146 147))

(test "symbolic-execution-2b"
  (run 10 (q)
    (fresh (a d)
      (== `(,a . ,d) q))
    (evalo
      `((lambda (n)
          (if (= 137 n)
              'foo
              'bar))
        ,q)
      'bar))
  '('138 '139 '140 '141 '142 '143 '144 '145 '146 '147))

(test "symbolic-execution-2c"
  (run 10 (q)
    (evalo
      `((lambda (n)
          (if (= 137 n)
              'foo
              'bar))
        ',q)
      'bar))
  '(138 139 140 141 142 143 144 145 146 147))


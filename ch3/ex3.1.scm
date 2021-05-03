(load "../sdf/manager/load")
(manage 'new 'combining-arithmetics)

(define boolean-arithmetic
  (make-arithmetic 'boolean boolean? '()
                   (lambda (name)
                     (case name
                       ((additive-identity) #f)
                       ((multiplicative-identity) #t)
                       (else (default-object))))
                   (lambda (operator)
                     (let ((procedure
                            (case operator
                              ((+) (lambda (b1 b2) (or b1 b2)))
                              ((*) (lambda (b1 b2) (and b1 b2)))
                              ((negate) (lambda (b) (not b)))
                              (else (lambda args (error "Operator undefined in Boolean" operator))))))
                       (simple-operation operator boolean? procedure)))))

(install-arithmetic! boolean-arithmetic)

(assert (eq? (+) #f))
(assert (eq? (*) #t))
(assert (eq? (- #t) #f))
(assert (eq? (- #f) #t))
(assert (eq? (+ #t #t) #t))
(assert (eq? (+ #t #f) #t))
(assert (eq? (+ #f #f) #f))
(assert (eq? (* #t #t) #t))
(assert (eq? (* #t #f) #f))

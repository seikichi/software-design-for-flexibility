(load "../sdf/manager/load")

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

(+)       ; #f
(*)       ; #t
(- #t)    ; #f
(- #f)    ; #t
(+ #t #t) ; #t
(+ #t #f) ; #t
(+ #f #f) ; #f
(* #t #t) ; #t
(* #f #t) ; #f

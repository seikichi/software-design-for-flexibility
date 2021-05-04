(load "../sdf/manager/load")
(manage 'new 'combining-arithmetics)

(register-predicate! vector? 'vector)

(define (ensure-vector-lengths-match vecs)
  (let ((first-vec-length (vector-length (car vecs))))
    (if (any (lambda (v)
               (not (n:= (vector-length v)
                         first-vec-length)))
             vecs)
        (error "Vector dimension mismatch:" vecs))))

(define (vector-element-wise element-procedure)
  (lambda vecs    ; Note: this takes multiple vectors
    (ensure-vector-lengths-match vecs)
    (apply vector-map element-procedure vecs)))

;; Ex. 3.2a
(define (vector-extender base-arithmetic)
  (let ((base-predicate (arithmetic-domain-predicate base-arithmetic)))
    (make-arithmetic
     'vector vector? (list base-arithmetic)
     (lambda (name base-constant) (default-object))
     (lambda (operator base-operation)
       (let ((base-proc (operation-procedure base-operation)))
         (case operator
           ((+ - negate) (simple-operation operator vector? (vector-element-wise base-proc)))
           (else #f)))))))

(install-arithmetic! (extend-arithmetic vector-extender combined-arithmetic))

(+ #(1 2 3) #(4 5 6))
(+ 'a 'b)
(- 1 2)
(- #(1 2 3) #(3 3 3))
(- #(4 5 6 'a))
(+ #(1 2 3) #(4 5 6))
(- #(4 5 6) #(1 1 1) #(1 2 3))

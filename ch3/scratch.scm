(load "../sdf/manager/load")

;; 3.1.1
(manage 'new 'combining-arithmetics)

(define (make-initial-history t h xt xt-h xt-2h)
  (list (cons t xt)
        (cons (- t h) xt-h)
        (cons (- t (* 2 h)) xt-2h)))

(define (extend-history t+h xt+h history)
  (cons (cons t+h xt+h) history))

(define (t index history)
  (car (list-ref history index)))

(define (x index history)
  (cdr (list-ref history index)))

(define (stormer-2 F h)
  (lambda (history)
    (+ (* 2 (x 0 history))
       (* -1 (x 1 history))
       (* (/ (expt h 2) 12)
          (+ (* 13 (F (t 0 history) (x 0 history)))
             (* -2 (F (t 1 history) (x 1 history)))
             (F (t 2 history) (x 2 history)))))))

(define (stepper h integrator)
  (lambda (history)
    (extend-history (+ (t 0 history) h)
                    (integrator history)
                    history)))

(define (evolver F h make-integrator)
  (let ((integrator (make-integrator F h)))
    (let ((step (stepper h integrator)))
      (define (evolve history n-steps)
        (if (n:> n-steps 0)
            (evolve (step history) (n:- n-steps 1))
            history))
      evolve)))

(define (F t x) (- x))

(define numeric-s0 (make-initial-history 0 .01 (sin 0) (sin -.01) (sin -.02)))

(x 0 ((evolver F .01 stormer-2) numeric-s0 100)) ;; .8414709493275624
(sin 1)                                          ;; .8414709848078965

;; 3.1.2
(install-arithmetic! symbolic-arithmetic-1)

(+ 'a 'b) ;; (+ a b)
(+ 1 2)   ;; (+ 1 2)
(x 0 ((evolver F 'h stormer-2) (make-initial-history 't 'h 'xt 'xt-h 'xt-2h) 1))

;; 3.1.3
(install-arithmetic! combined-arithmetic)
(+ 1 2)
(+ 'a 'b)
(+ 'a 2)
(x 0 ((evolver F 'h stormer-2) (make-initial-history 't 'h 'xt 'xt-h 'xt-2h) 1))
(x 0 ((evolver F 'h stormer-2) numeric-s0 1))

;; 3.1.4
(install-arithmetic! (extend-arithmetic pure-function-extender combined-arithmetic))
((+ cos sin) 3)
((+ cos sin) 'a)

(install-arithmetic! (extend-arithmetic function-extender combined-arithmetic))
((+ 1 cos) 'a)
(* 'b ((+ (literal-function 'c) cos sin) (+ (+ 1 2) 'a)))

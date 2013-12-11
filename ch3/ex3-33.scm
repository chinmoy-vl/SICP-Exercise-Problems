(load "make-connector.scm")
(load "primitive-constraints.scm")

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(define (averager a b c)
  (let 
    ((x (make-connector))
    (w (make-connector)))
    (adder a b x)
    (multiplier w c x)
  (constant 2 w)))

(averager a b c)


;(probe "average value" c)
;Value 24: #[compound-procedure 24 me]

;(set-value! a 10 'user)
;Value: done

;(set-value! b 40 'user)
;Probe: average value = 25
;Value: done

;(set-value! b 80 'user)
;Contradiction (40 80)


;(forget-value! b 'user)
;Probe: average value = ?
;Value: done

;(set-value! b 80 'user)
;Probe: average value = 45
;Value: done
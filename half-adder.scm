(load "make-wire.scm")

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
  	(or-gate ab d)
  	(and-gate a b c)
  	(inverter c e)
  	(and-gate d e s)
  	'ok))
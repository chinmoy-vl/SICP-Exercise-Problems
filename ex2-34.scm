

(define (accumulate op initial sequnce)
  (if (null? sequnce)
      initial
      (op (car sequnce)
      		(accumulate op initial (cdr sequnce)))))	


(define (homer-eval x coefficient-squence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
  						0
  						coefficient-squence))
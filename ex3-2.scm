(define (make-monitered f)
	(define times  0)
	(define (mf m)
	  (cond ((eq? m 'how-many-calls?) times)
	  			((eq? m 'reset-count) (begin (set! times 0) times))
	        (else (begin (set! times (+ times 1)) (f m)))))
	mf)

(define b (make-monitered sqrt))


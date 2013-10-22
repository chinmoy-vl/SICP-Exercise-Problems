(define (double f)
	(lambda (y) 
		(f (f y))))

(define (inc a)
	(+ a 1))
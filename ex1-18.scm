(define (multiply a b)
	(cond ((= a 1) b)
		( (even? a) ( multiply (halve a) (double b)))
		(else (+ (multiply (halve (- a 1)) (double b)) b))
		))

(define (even? n)
	(= (remainder n 2) 0))

(define (halve a)
	(/ a 2))

(define (double b)
	(* b 2))
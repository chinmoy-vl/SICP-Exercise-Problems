(define (accumulate combiner null-value term a next b)
	(if(> a b)
			null-value
			(combiner (term a) 
				(accumulate combiner null-value term (next a) next b))))


(define (accumulate-iter combiner null-value term a next b)
	(define (iterate a result)
	(if(> a b)
			result
			(combiner (term a) 
				(accumulate combiner null-value term (next a) next b))))
	(iterate a null-value))

(define (sum n)
	(define (combiner a b)
		(+ a b))
	(define (term n) 
		n)
	(define (next n )
		(+ n 1))
	(accumulate-iter combiner 0 term 1 next n))

(define (factorial n)
	(define (combiner a b)
		(* a b))
	(define (term n) 
		n)
	(define (next n )
		(+ n 1))
	(accumulate-iter combiner 1 term 1 next n))
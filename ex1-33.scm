(define (filtered-accumulate filter? combiner null-value term a next b)
	(if(> a b)
			null-value
			(if (filter? a)
				(combiner (term a) 
									(filtered-accumulate filter? combiner null-value term (next a) next b))
				(filtered-accumulate filter? combiner null-value term (next a) next b))))



(define (prime? n)
	(= n (smallest-divisor n)))

(define (smallest-divisor n)
	(find-divisor n 2))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
		((divides? n test-divisor) test-divisor)
		(else (find-divisor n (next-divisor test-divisor)))))

(define (next-divisor n)
	(if (= n 2)
		3 
		(+ n 2))
	)

(define (square n)
	(* n n))

(define (divides? n divisor)
	(= (remainder n divisor) 0))

(define (sum-sq-prime a b)
	(define (combiner a b)
		(+ a b))
	(define (term n) 
		(* n n))
	(define (next n )
		(+ n 1))
	(filtered-accumulate prime? combiner 0 term a next b))
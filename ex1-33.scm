;accumulate procedure with filter
(define (filtered-accumulate filter? combiner null-value term a next b)
	(if(> a b)
			null-value
			(if (filter? a)
				(combiner (term a) 
									(filtered-accumulate filter? combiner null-value term (next a) next b))
				(filtered-accumulate filter? combiner null-value term (next a) next b))))


;test for prime numbers
(define (prime? n)
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
(define (divides? n divisor)
	(= (remainder n divisor) 0))
	(= n (smallest-divisor n)))



(define (square n)
	(* n n))

;to find the sum of squrare of prime numbers within a given range
(define (sum-sq-prime a b)
	(define (combiner a b)
		(+ a b))
	(define (term n) 
		(* n n))
	(define (next n )
		(+ n 1))
	(filtered-accumulate prime? combiner 0 term a next b))


(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))))

(define (filter-relatively-prime a n)
	(= (gcd a n) 1))

(define (rel-prime-prod n)
	(define (product a b)
		(* a b))
	(define (term a)
		a)
	(define (next n)
		(+ n 1))
	(define (relatively-prime? a)
		(filter-relatively-prime a n))
	(* (filtered-accumulate relatively-prime? product 1 term 1 next n) n))

;
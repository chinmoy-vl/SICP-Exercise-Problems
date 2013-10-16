(define (new-if predicate then-clause else-clause)
	(cond (predicate then-clause)
		(else else-clause)))

(define (sqrt x)
	(sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
	(new-if (good-enough guess x)
		guess
		(sqrt-iter (improve-guess guess x) 
			x)))

(define (improve-guess guess x)
	(average guess (/ x guess)))

(define (average a b)
	(/ (+ a b) 2.0))

(define (good-enough guess x)
	(< (abs (- (square guess) x)) 0.001))

(define (square x)
	(* x x))
(define (sqrt x)
	(sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
	(if (good-enough guess (improve-guess guess x))
		guess
		(sqrt-iter (improve-guess guess x) 
			x)))

(define (improve-guess guess x)
	(average guess (/ x guess)))

(define (average a b)
	(/ (+ a b) 2.0))

(define (good-enough guess imporvedguess )
	(< (abs (- guess imporvedguess)) 0.00001))

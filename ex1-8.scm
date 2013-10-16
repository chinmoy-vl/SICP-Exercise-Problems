(define (cube-root x)
	(cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
	(if (good-enough? guess (improve-guess guess x))
		guess
		(cube-root-iter (improve-guess guess x) x)))

(define (improve-guess guess x)
	(/ (+ (/ x (square guess))
			(* 2 guess))
		3))

(define (good-enough? guess improvedguess)
	(< (abs (- guess improvedguess)) 0.0001))
(define (compose f g)
	(lambda (x) (f (g x))))

( define (inc a)
	(+ a 1))

(define (repeated f count)
	(if (> count 1)
		(compose f (repeated f (- count 1)))
		(lambda (x) (f x))) )

(define (square x) (* x x))
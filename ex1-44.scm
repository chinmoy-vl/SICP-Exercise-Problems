(define dx 0.0001)

(define (smooth f)
	(lambda (x) (/ ((f (+ x dx))
											(f x)
											(f (- x dx))) 3)))

(define (compose f g)
	(lambda (x) (f (g x))))

(define (repeated f count)
	(if (> count 1)
		(compose f (repeated f (- count 1)))
		(lambda (x) (f x))) )

(define (repeated-smooth f count)
	(repeated smooth count)e)

(define pi (/ 22 7))



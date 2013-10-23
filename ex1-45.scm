;((define (n-root x n)
 ; (lambda (y) (/ x (expt y (- n 1))))))


(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? next guess)
				next
				(try next))))
	(try first-guess))


(define (compose f g)
	(lambda (x) (f (g x))))

(define (repeated f count)
	(if (> count 1)
		(compose f (repeated f (- count 1)))
		(lambda (x) (f x))) )

(define (average a b)
	(/ (+ a b) 2))

(define (average-damp f)
  (lambda(x)
    (average x (f x))))

(define (n-root x n)
  (fixed-point (repeated (average-damp (expt x n)) 2) 1.0))

(display (n-root 256 8))
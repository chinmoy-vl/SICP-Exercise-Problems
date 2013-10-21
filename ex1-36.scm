(define tolerance 0.0001)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(let ((next (f guess)))
		(newline)
		(display next)
		(if (close-enough? guess next)
			next
			(try next))))
	(try first-guess))


(define (average a b)
	(/ (+ a b) 2))

;without average damping
;(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

;with average damping
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
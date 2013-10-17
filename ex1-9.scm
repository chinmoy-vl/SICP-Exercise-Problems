(define (+ a b)
	(if (= a 0)
		b
		(inc (+ (dec a ) b))))

(define (dec a)
	(- a 1))

(define (inc a)
	(+ a 1))



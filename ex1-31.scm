(define (product term a next b)
	(if (> a b)
		1
		(* (term a)
			(product term (next a) next b))))

(define (product-iter term a next b)
	(define (iter a result)
	(if (> a b)
		result
		(iter (next a) (* (term a) result))))
	(iter a 1))


(define (fact x)
	(define (fact-term x)
		(if (= x 0)
			1
			x))
	(define (next a)
		(+ a 1))
	(product fact-term 1 next x))


#| (define (wallis-pi n)
	(define (pi-term n count)
		(cond ((= n 1)
			(/ 2.0 3.0))
			((and (> n 1) (= count 0))
				(/ (* 2 n) (- (* 2 n) 1)))
			((and (> n 1) (= count 1))
				(/ (* 2 n) (+ (* 2 n) 1)))))
	(define (next-n n)
			(+ n 1))
	(define (pi-term-caller n)
		(cond ((even? n)
				(pi-term n 0))
			(else (pi-term n 1))))
	(define (even? n)
		 (= (remainder n 2) 0))
	(* 4 (product pi-term-caller 1 next-n n)))
 |#


(define (wallis n)
	(define (next n)
		(+ n 1))
	(define (pi-term n)
		(/ (* 4.0 n n) (* (- (* 2.0 n) 1) (+ (* 2.0 n) 1))))
	(* (product-iter pi-term 1 next 100) 2))
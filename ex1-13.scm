(define (const_eps)
	(/ (- 1 (sqrt 5)) 2))

(define (const_phy)
	(/ (+ 1 (sqrt 5)) 2))

(define (fib n)
	(/ (iterative-expo (const_phy) n) (sqrt 5)))

(define (iterative-expo b n)
  (define (iterate a b n)
    (cond ((= n 0) a)
          ((even? n) (iterate a (square b) (/ n 2)))
          (else (iterate (* a b) b (- n 1)))))
  (iterate 1 b n))

(define (sqrt x)
	(sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
	(if (good-enough guess x)
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
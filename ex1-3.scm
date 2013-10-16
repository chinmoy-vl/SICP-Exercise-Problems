(define (sum-of-sq a b) (+ (square a) (square b)))
(define (square a) (* a a))
(define (sum-of-sq-larger-no a b c) 
	(cond ((and (> a b) (> b c) ) (sum-of-sq a b))
				((and (> a b) (> c b)) (sum-of-sq a c) )
				(else (sum-of-sq b c))))


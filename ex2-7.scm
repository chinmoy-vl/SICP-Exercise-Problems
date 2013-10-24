(define (make-interval a b)
  (cons a b))

(define (upper-bound x)
  (if (> (car x) (cdr x) )
  		(car x)
  		(cdr x)))

(define (lower-bound x)
  (if (< (car x) (cdr x) )
  		(car x)
  		(cdr x)))
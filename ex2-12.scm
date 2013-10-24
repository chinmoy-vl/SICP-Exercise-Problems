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

(define (add-interval x y)
	make-interval (+ (lower-bound x) (lower-bound y))
										(upper-bound x) (upper-bound y))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
  			(p2 (* (lower-bound x) (upper-bound y)))
  			(p3 (* (upper-bound x)	(lower-bound y)))
  			(p4 (* (upper-bound x) 	(upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
									(max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (or (= (lower-bound y) 0) (= (upper-bound y) 0))
    (error "upper bound or lower bound of an interval cannot be zero")
    (mul-interval x
  							 (make-interval (/ 1.0 (upper-bound y))
  															(/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
  (make-interval (abs (- (lower-bound x) (lower-bound y)))
                  (abs (- (upper-bound x) (upper-bound y)))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/(- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percent)
  (make-interval (- center (* center (/ percent 100)))
                 (+ center (* center (/ percent 100)))))
(define (percent i)
  (* (/ (width i) (center i)) 100))



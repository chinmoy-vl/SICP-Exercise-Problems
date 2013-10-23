(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (distance x y)
  (+ x y))

(define (print-point p)
  (newline)
	(display "(")
	(display (x-point p))
	(display ",")
	(display (y-point p))
	(display ")"))	

(define (midpoint-segment segment)
  (make-point (/ (distance (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
  						(/ (distance (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

(define (check-midpoint x1 y1 x2 y2)
	(print-point (midpoint-segment (make-segment (make-point x1 y1) (make-point x2 y2)))))
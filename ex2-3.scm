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

(define (print-point p)
  (newline)
	(display "(")
	(display (x-point p))
	(display ",")
	(display (y-point p))
	(display ")"))

(define (make-rect p1 p2 p3 p4)
  (cons (make-segment p1 p2) (make-segment p3 p4)))

(define (rect-length r)
  (abs (-(car (car (car r)))
          (car (cdr (car r))))))

(define (rect-breadth r)
  (abs (-(cdr (cdr (car r)))
          (cdr (cdr (cdr r))))))


(define (perimeter r)
  (display (* (+ (rect-length r) (rect-breadth r)) 2)))

(define (area r)
  (display (* (rect-length r) (rect-breadth r))))

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
  						(/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

(define (check-midpoint x1 y1 x2 y2)
	(print-point (midpoint-segment (make-segment (make-point x1 y1) (make-point x2 y2)))))

(define rect (make-rect (make-point 2 4) (make-point 8 4) (make-point 8 2) (make-point 2 2)))

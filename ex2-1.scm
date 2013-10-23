(define (add-rat x y)
	(make-rat (+ (* (numer x) (denom y)
									(numer y)	(denom x))
								(* (denom x) (denom y)))))

(define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y)
									(numer y)	(denom x))
								(* (denom x) (denom y)))))

(define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
						(* (denom x) (denom y))))

(define (mul-rat x y)
	(make-rat (* (numer x) (denom y))
						(* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
  		(denom x) (numer y)))

(define (gcd a b)
  (if (= (remainder a b) 0)
  		b
  		(gcd b (remainder a b))))


(define (make-rat n d)
	(let ((g (abs (gcd n d))))
		(if (< d 0)
			(cons (/ (* n -1) g)
						(/ (* d -1) g))
			(cons (/ n g) (/ d g)))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
	(display (numer x))
	(display "/")
	(display (denom x)))


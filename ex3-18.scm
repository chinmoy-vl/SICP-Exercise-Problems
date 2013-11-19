(define (test-cycle? l)
  (if (null? (cdr l))
      (display "Not a cycle")
      (let ((first (car l)))
      	(define (loop x)
  				;(display x)
      	  (cond ((null? x) (display "Not a cycle"))
    						((eq? (car x) (car l)) (display "cycle"))
        				(else (loop (cdr x)))))
      	(loop (cdr l)))))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
	x)


(define z	(make-cycle (list 'a 'b 'c)))
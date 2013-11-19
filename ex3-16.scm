(define (count-pairs x)
  (if (not (pair? x))
      0
      (+	(count-pairs (car x))
      		(count-pairs (cdr x))
      	1)))

(define x (list 'a 'b 'c))
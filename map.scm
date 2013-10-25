
(define (map proc l)
  (if (null? l)
  		()
  		(cons (proc (car l))
  		(map proc (cdr l)))))

(map	(lambda (y) (* y y))
			(list 1 2 3 4 5 6))


(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (even? x)
  (= (remainder x 2) 0))

(define (same-parity a . b)	
	(if (even? a)
			(filter-list (append (list a) b) 0)
			(filter-list (append (list a) b) 1)))

(define (filter-list l parity)
  (if (null? l)	
      l
  		(if (=  (remainder (car l) 2) parity)
  						(append (list (car l)) (filter-list (cdr l) parity))
  						(filter-list (cdr l) parity))))
(define (last-pair list)
  (if (null? (cdr list))
  		(display  list)
  		(last-pair (cdr list))))
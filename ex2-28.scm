(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))


(define (fringe items)
	(define (fringe-iter items answer)
  (cond ((null? items) answer)
        ((not (pair? items)) (append (list items) answer))
      	(else (append (fringe-iter (car items) ()) (fringe-iter (cdr items) ())))))
	(fringe-iter items ()))


(define x
  (list (list 1 2) (list 3 4)))

(fringe x)
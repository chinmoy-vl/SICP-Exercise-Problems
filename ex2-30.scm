

(define (square x)
  (* x x))

(define (square-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (square tree))
      	(else (cons (square-tree (car tree))
      							(square-tree (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
  				(if (pair? sub-tree)
  								(square-tree2 sub-tree)
  								(square sub-tree)))
				tree))	

(define x
				(list 1 
								(list 2 (list 3 4) 5)
								(list 6 7)))
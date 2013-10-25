

(define (map proc l)
  (if (null? l)
      ()
      (cons (proc (car l))
      (map proc (cdr l)))))

(define (square x)
  (* x x))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
  				(if (pair? sub-tree)
  								(square-tree2 sub-tree)
  								(proc sub-tree)))
				tree))	

(define x
				(list 1 
								(list 2 (list 3 4) 5)
								(list 6 7)))

(define (square-tree tree)
  (tree-map square tree))
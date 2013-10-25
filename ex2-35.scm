


(define (enumerate-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
      	(else (append (enumerate-tree (car tree))
      				 (enumerate-tree (cdr tree))))))


(define (accumulate op initial sequnce)
  (if (null? sequnce)
      initial
      (op (car sequnce)
      		(accumulate op initial (cdr sequnce)))))	

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (+ x 1)) (enumerate-tree t))))


(define x
				(list 1 
								(list 2 (list 3 4) 5)
								(list 6 7)))
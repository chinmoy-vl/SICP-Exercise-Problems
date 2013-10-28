
(define (accumulate op initial sequnce)
  (if (null? sequnce)
      initial
      (op (car sequnce)
      		(accumulate op initial (cdr sequnce)))))	

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
  		()
  		(cons (accumulate op init (map car seqs))
  					(accumulate-n op init (map cdr seqs) ))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (accumulate + 0 (map * v x) )) m))

(define (transpose mat)
  (accumulate-n cons (list) mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
        (map (lambda (row) (matrix-*-vector cols row)) m)))  




(define m (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define n (list (list 10 11 12) (list 13 14 15) (list 16 17 18)))
(define v (list 2 3 4))
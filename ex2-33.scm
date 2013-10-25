
(define (accumulate op initial sequnce)
  (if (null? sequnce)
      initial
      (op (car sequnce)
      		(accumulate op initial (cdr sequnce)))))	

(define (map p sequnce)
  (accumulate (lambda (x y) (cons (p x) y)) (list) sequnce))


(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequnce)
  (accumulate (lambda (x y) (+ y 1)) 0 sequnce))

(define (square x)
  (* x x))

(define (inc x)
  (+ 1 x))
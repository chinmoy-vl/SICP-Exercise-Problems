

(define (accumulate op initial sequnce)
  (if (null? sequnce)
      initial
      (op (car sequnce)
      		(accumulate op initial (cdr sequnce)))))	

(define (enumerate-interval start end)
  (if (> start end)
  			()
  			(cons start (enumerate-interval (+ start 1) end))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))


(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (cons i j)) 
  													(enumerate-interval 1 (- i 1))))
  					(enumerate-interval 1 n)))

(define (triples n s)
  (flatmap (lambda (k) (map (lambda (pair) (list k (car pair) (cdr pair))) 
  													(filter (lambda (pair) (= (+ (car pair) (cdr pair) k) s)) (unique-pairs k) )))
						(enumerate-interval 1 (- n 1))))
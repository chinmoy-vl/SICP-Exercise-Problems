

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

(define (prime? n)
	(= n (smallest-divisor n)))

(define (smallest-divisor n)
	(find-divisor n 2))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
		((divides? n test-divisor) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))

(define (divides? n divisor)
	(= (remainder n divisor) 0))

(define (square n)
(* n n))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cdr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cdr pair) (+ (car pair) (cdr pair))))

(define (prime-sum-pairs n)	
  (map make-pair-sum
  	(filter prime-sum? (unique-pairs n))))


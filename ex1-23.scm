(define (timed-prime-test n)
		(newline)
		(display n)
		(start-prime-test n (runtime)))

(define (even? n)
	(= (remainder n 2) 0))

(define (start-prime-test n start-time)
	(if(fast-prime? n 100)
		(report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time))

(define (prime? n)
	(= n (smallest-divisor n)))

(define (smallest-divisor n)
	(find-divisor n 2))

(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
		((divides? n test-divisor) test-divisor)
		(else (find-divisor n (next-divisor test-divisor)))))

(define (next-divisor n)
	(if (= n 2)
		3 
		(+ n 2))
	)

(define (divides? n divisor)
	(= (remainder n divisor) 0))

(define (square n)
	(* n n))



(define (expmod base exp m)
   (cond ((= exp 0) 1)
         ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                     m))
         (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
   (define (try-it a)
     (= (expmod a n n) a))
   (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
   (cond ((= times 0) true)
         ((fermat-test n) (fast-prime? n (- times 1)))
         (else false)))

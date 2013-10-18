(define (sfp start end)
	(cond ((even? start) (search-iter (+ start 1) end))
		(else (search-iter start end))))

(define (search-iter start end)
	(timed-prime-test start)
	(cond ((< start end)  (search-iter (+ start 2) end))
		))


(define (even? n)
	(= (remainder n 2) 0))

(define (timed-prime-test n)
		(newline)
		(display n)
		(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
	(if(prime? n)
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
		(else (find-divisor n (+ test-divisor 1)))))

(define (divides? n divisor)
	(= (remainder n divisor) 0))

(define (square n)
	(* n n))
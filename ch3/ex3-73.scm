
(define i
  (i-stream 1))
(define (i-stream i)
  (if (> i 100)
      the-empty-stream
  		(cons-stream i (i-stream (+ i 1)))))


;(define (scale-stream stream factor)
;  (stream-map 
;  	(lambda (x) (* x factor))
;  	stream))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream	initial-value
    							(add-streams (scale-stream integrand dt)
    													int)))
	int)	

(define (RC r c dt)
  (lambda (i init-val)
   	(add-streams
   			(scale-stream i r)
   	   	(integral 
   	   		(scale-stream i (/ 1 c))
   	   		init-val
   	   		dt))))

(define RC1 (RC 5 1 0.5))

(define (generate-stream stream n)
  (define (generate-stream-iter stream start limit)
    (cond ((< start limit ) (display (stream-car stream)) 
                    (newline) 
                    (generate-stream-iter (stream-cdr stream) (+ start 1) limit))
        (else (display "done"))))
  (generate-stream-iter stream 0 n))

;(generate-stream (RC1 i-stream 2.0) 10)

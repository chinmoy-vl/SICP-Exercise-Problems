
(define (scdr str)  
  ((cdr str)))

(define (integers n)
  (cons n (lambda () (integers (+ n 1)))))


(define (fibs-iterator a b)
  (cons a (lambda () (fibs-iterator b (+ a b)))))

;fibonacci number
(define (fibs)
  (fibs-iterator 0 1))

(define (s-ref s n)
    (cond ((= n 0) (car s))
          (else (s-ref (scdr s) (- n 1)))))


(define (first-twenty)
  (generate-stream (fibs) 20))

(define (generate-stream stream n)
  (define (generate-stream-iter stream start limit)
    (cond ((< start limit ) (display (car stream)) 
                    (newline) 
                    (generate-stream-iter (scdr stream) (+ start 1) limit))
        (else (display "done"))))
  (generate-stream-iter stream 0 n))
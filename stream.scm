
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
  (define (iterate n)
    (cond ((< n 20 ) (display (s-ref (fibs) (+ n 1))) 
                      (newline) 
                      (iterate (+ n 1)))
          (else (display "done"))))
  (iterate 0))
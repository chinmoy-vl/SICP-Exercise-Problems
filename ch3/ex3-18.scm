;(define (check-cycle x)
;  (let ((first (car x)))
;    (define (iter list)
;      (cond ((null? (cdr list)) 
;                #f)
;            ((eq? first (car list)) 
;                #t)
;            (else 
;                (iter (cdr list)))))
;    (iter (cdr x))))

(define (check-cycle? x) 
  (let ((alredy-chked '()))
  (define (iter list) 
    (set! alredy-chked (cons list alredy-chked)) 
    (cond ((null? (cdr list)) false) 
          ((memq (cdr list) alredy-chked) true) 
          (else (iter (cdr list))))) 
  (iter x)))

  (define (make-cycle x)
    (define (last-pair x)
      (if (null? (cdr x))
          x
          (last-pair (cdr x))))
  (set-cdr! (last-pair x) x)
  x)

;(define z (make-cycle (list 'a 'b 'c)))
;Value: z

;(check-cycle? z)
;Value: #t

;(check-cycle? (list 1 2 3))
;Value: #f

;(check-cycle? (list 1 2 3 1))
;Value: #f

;check-cycle? (list 1 2 3 1))
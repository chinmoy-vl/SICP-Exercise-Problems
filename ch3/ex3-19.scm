(define (check-cycle? x)
  (define (cdr2 x)
    (if (null? x)
      ()
      (cdr x)))
  (define (iterate hare rabbit)
    (cond ((null? rabbit) #f)
          ((eq? hare rabbit) #t)
          (else (iterate (cdr2 hare) (cdr2 (cdr2 rabbit))))))
  (iterate (cdr2 x) (cdr2 (cdr2 x))))



  (define (make-cycle x)
    (define (last-pair x)
      (if (null? (cdr x))
          x
          (last-pair (cdr x))))
  (set-cdr! (last-pair x) x)
  x)


;(check-cycle? (list '1 '2 '3 '1))
;Value: #f

;(check-cycle? (list 1 2 3))
;Value: #f

;(define z (make-cycle (list 'a 'b 'c)))
;Value: z

;(check-cycle? z)
;Value: #t
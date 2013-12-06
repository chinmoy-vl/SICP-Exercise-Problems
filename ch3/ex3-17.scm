(define (count-pairs x)
  (let ((counted-pairs '()))
    (define (iter list)
      (if 
        (or
            (not (pair? list))
            (memq (car list) counted-pairs))
        0
        (begin
          (set! 
            counted-pairs
              (cons (car list) counted-pairs))
          ;(display counted-pairs)
          (+
            (iter (car list))
            (iter (cdr list))
            1))))
    (iter x)))


;(count-pairs (cons 'a (cons 'b (cons 'c 'd))))
;Value: 3

; (define a (cons 'x 'y))
; (define b (cons 'm 'n))
; (define c (cons 'a '()))
; (set-cdr! a b)
; (set-car! a c)
; (set-cdr! b c)

;(count-pairs a)
;Value: 3

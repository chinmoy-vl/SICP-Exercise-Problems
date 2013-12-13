(load "common-stream-proc.scm")


(define (integrate-series s1)
  (stream-map / s1 integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))



(define (mul-series s1 s2)
  (cons-stream (*
                (stream-car s1)
                (stream-car s2))
              (add-streams
                (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2))))

; generate the negatives of a stream
(define (negate-stream s)
  (stream-map 
    (lambda (x) (* -1 x))
    s))

(define (invert-unit-series stream)
  (define invert
    (cons-stream 
      1
      (negate-stream
        (mul-series
          (stream-cdr stream)
          invert))))
  invert)

; (show-series (invert-unit-series exp-series) 10)
; 1
; -1
; 1/2
; -1/6
; 1/24
; -1/120
; 1/720
; -1/5040
; 1/40320
; -1/362880
;Value: done



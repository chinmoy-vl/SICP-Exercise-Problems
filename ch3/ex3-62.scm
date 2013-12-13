(load "common-stream-proc.scm")


(define (integrate-series s1)
  (stream-map / s1 integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1
                (integrate-series
                  (stream-map 
                    (lambda (x) (* -1 x ))
                    sine-series))))

(define sine-series
  (cons-stream 0 
              (integrate-series cosine-series)))


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


(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "Division by zero")
      (mul-series
        s1
        (invert-unit-series s2))))

;(show-series 
  ;(div-series exp-series exp-series) 
  ;10)
;1
;0
;0
;0
;0
;0
;0
;0
;0
;0

;(show-series 
  ;(div-series cosine-series sine-series) 
  ;10)
;;Division by zero

;(show-series 
;(div-series sine-series cosine-series) 
;10)
;0
;1
;0
;1/3
;0
;2/15
;0
;17/315
;0
;62/2835
;Value: done
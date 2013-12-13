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


(define verify
  (add-streams
    (mul-series sine-series sine-series)
    (mul-series cosine-series cosine-series)))


;(show-series verify 10)
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
;Value: done
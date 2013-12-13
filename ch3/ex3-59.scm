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



;(show-series exp-series 10)
;1
;1
;1/2
;1/6
;1/24
;1/120
;1/720
;1/5040
;1/40320
;1/362880


;(show-series sine-series 10)
;0
;1
;0
;-1/6
;0
;1/120
;0
;-1/5040
;0
;1/362880
;Value: done

;(show-series cosine-series 10)
;1
;0
;-1/2
;0
;1/24
;0
;-1/720
;0
;1/40320
;0
;Value: done
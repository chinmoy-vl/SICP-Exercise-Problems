(load "common-stream-proc.scm")

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (cons-stream 1.0
                (stream-map (lambda (guess)
                              (sqrt-improve guess x))
                            (sqrt-stream x))))

(define (stream-limit stream tolerance)
  (if (<
        (abs 
          (- 
            (stream-car stream)
            (stream-car (stream-cdr stream))))
        tolerance)
      (stream-car (stream-cdr stream))
      (stream-limit (stream-cdr stream) tolerance)))


;(stream-limit (sqrt-stream 3) 0.000001)
;Value: 1.7320508075688772

;(stream-limit (sqrt-stream 300) 0.000000000000001)
;Value: 17.32050807568877

;(stream-limit (sqrt-stream 2500) 0.000000000000001)
;Value: 50.




(define (enumurate-stream low high)
  (if (> low high)
      the-empty-stream
      (cons-stream 
          low
          (enumurate-stream (+ low 1) high))))

(define (stream-enumurate-interval low high)
  (enumurate-stream low high))



(define (show x)
  (display x)
  (newline)
  x)

(define x (stream-map show (stream-enumurate-interval 0 10)))

;(stream-ref x 5)
;1
;2
;3
;4
;5
;Value: 5

;(stream-ref x 7)
;6
;7
;Value: 7
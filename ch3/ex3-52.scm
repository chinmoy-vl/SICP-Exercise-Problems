
(define (enumerate-stream low high)
  (if (> low high)
      the-empty-stream
      (cons-stream 
          low
          (enumerate-stream (+ low 1) high))))

(define (stream-enumerate-interval low high)
  (enumerate-stream low high))


(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))


(define (show-series stream n)
  (if (= n 0)
      'done
      (begin
        (display (stream-car stream))
        (newline)
        (show-series (stream-cdr stream) (- n 1)))))


;(stream-ref y 7)
;Value: 136

;(show-series z 20)
;10
;15
;45
;55
;105
;120
;190
;210

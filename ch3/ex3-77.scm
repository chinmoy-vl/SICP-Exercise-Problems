
(define (force delayed-object)
  (delayed-object))

(define (delay exp)
  (lambda () exp))

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? delayed-integrand)
                     the-empty-stream
                     (integral (stream-cdr delayed-integrand)
                               (+ (* dt (stream-car delayed-integrand))
                                  initial-value)
                               dt)))))


(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)


;(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
(load "common-stream-proc.scm")


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


(define (solve2 f y0 dy0 dt)
  (define y 
    (integral (delay dy) y0 dt))
  (define dy 
    (integral (delay ddy) dy0 dt))
  (define ddy 
    (stream-map y dy ))
  y)

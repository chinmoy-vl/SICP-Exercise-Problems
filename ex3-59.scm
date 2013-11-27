(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (integrate-series stream)
  (stream-map * stream (stream-map (lambda (x) (/ 1 x)) integers)))

(define exp-series (cons-stream 1 (integrate-series exp-series)))

(define (scale-series stream factor)
  (lambda (x) (* x factor)) stream)

(define cosine-series (cons-stream 1 (integrate-series (scale-series sine-series -1))))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))


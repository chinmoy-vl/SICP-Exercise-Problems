(define (average x y)
  (/ (+ x y) 2))


(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0 
  							(stream-map (lambda (guess)
  														(sqrt-improve guess x))
  													guesses)))
	guesses)

(define (stream-limit stream tolerance)
  (define (iter init-val strm)
    (let ((s (stream-car strm)))
      (if (> tolerance (abs (- s init-val)))
          s
          (iter s (stream-cdr strm)))))
  (iter (stream-car stream) (stream-cdr stream)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
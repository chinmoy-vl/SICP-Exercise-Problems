

(define (make-zero-crossings input-stream last-avpt-value)
    (cons-stream (sign-change-detector (stream-car input-stream) last-value)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream))))

(define (average a b)
  (/ (+ a b)
     2))


(define (smooth input-stream)
 (define (smooth-iter a b stream)
    (cons-stream 
      (average a b)
      (smooth-iter
        (stream-car input-stream)
        (stream-car (stream-cdr input-stream))
        (stream-cdr input-stream))))
  (smooth-iter
    (stream-car input-stream)
    (stream-car (stream-cdr input-stream))
    (stream-cdr input-stream)))


(define zero-crossings
  (make-zero-crossings 
    (smooth (cons 0 sense-data)) 
    0))
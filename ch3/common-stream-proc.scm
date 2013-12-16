(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map 
                      stream-car
                      argstreams))
        (apply stream-map
               (cons proc (map
                            stream-cdr
                            argstreams))))))

(define (scale-stream stream factor)
  (stream-map 
    (lambda (x)
      (* x factor))
    stream))

(define (show-series stream n)
  (if (= n 0)
      'done
      (begin
        (display (stream-car stream))
        (newline)
        (show-series (stream-cdr stream) (- n 1)))))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (integers-from-n n)
  (cons-stream n (add-streams 
                    ones 
                    (integers-from-n n)))) 
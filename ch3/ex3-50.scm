(define ones
  (cons-stream 1 ones))  

(define (int-iter i)
  (cons-stream
    i
    (int-iter (+ i 1))))

(define integers 
  (int-iter 1))

(define threes
  (cons-stream 3 threes))  


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


(define (show-series stream n)
  (if (= n 0)
      'done
      (begin
        (display (stream-car stream))
        (newline)
        (show-series (stream-cdr stream) (- n 1)))))

;(stream-ref integers 10)
;11

;(show-series x 10)
;5
;6
;7
;8
;9
;10
;11
;12
;13
;14
;Value: done

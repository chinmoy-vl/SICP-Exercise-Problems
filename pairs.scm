(define ones
  (cons-stream 1 ones))
(define twos
  (cons-stream 2 twos))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define odds 
  (cons-stream 1 
                (add-streams twos odds)))

(define evens 
  (cons-stream 2 
                (add-streams twos evens)))

(define integers 
  (cons-stream 1 
                (add-streams ones integers)))


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
      							(interleave s2 (stream-cdr s1)))))


(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


(define (generate-stream stream n)
  (define (generate-stream-iter stream start limit)
    (cond ((< start limit ) (display (stream-car stream)) 
                    (newline) 
                    (generate-stream-iter (stream-cdr stream) (+ start 1) limit))
        (else (display "done"))))
  (generate-stream-iter stream 0 n))

(define (first-n n)
  (generate-stream (pairs odds evens) n))

(define int-pair
  ;(pairs integers integers))
  (pairs finite-int finite-int))

(define (stream-enumurate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumurate-interval (+ low 1) high))))

(define  finite-int
  (stream-enumurate-interval 1 100))

(define (display-all stream)
  (if (stream-null? stream)
      (display "Done")
      (begin 
        (display (stream-car stream))
        (newline)))
  (display-all (stream-cdr stream)))


(define (count-elements-before-pair pair-car pair-cdr stream)  
  (define (iterate pair-car pair-cdr stream count)
  (if (> pair-car pair-cdr)
      (display "Invalid Pair(i,j), i must be <=  j ")
      (cond ((and (=  pair-car (car (stream-car stream)))
                  (=  pair-cdr (cadr (stream-car stream)))) 
            (display "Count: ")
            (display (car count)))
            (else
              (iterate pair-car pair-cdr (stream-cdr stream) (stream-cdr count))))))
  (iterate pair-car pair-cdr stream integers))





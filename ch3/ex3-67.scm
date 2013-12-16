(define ones
  (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers 
  (cons-stream 1 
                (add-streams ones integers)))


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
      							(interleave s2 (stream-cdr s1)))))



(define (pairs2 s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave 
              (interleave
                         (stream-map (lambda (x) (list (stream-car s) x))
                                     (stream-cdr t))
                         (stream-map (lambda (x) (list x (stream-car t)))
                                     (stream-cdr s)))
              (pairs2 (stream-cdr s) (stream-cdr t)))))


(define (generate-stream stream n)
  (define (generate-stream-iter stream start limit)
    (cond ((< start limit ) (display (stream-car stream)) 
                    (newline) 
                    (generate-stream-iter (stream-cdr stream) (+ start 1) limit))
        (else (display "done"))))
  (generate-stream-iter stream 0 n))

(define (first-n n)
  (generate-stream (pairs2 integers integers) n))



; (first-n 10)
; (1 1)
; (1 2)
; (2 2)
; (2 1)
; (2 3)
; (1 3)
; (3 3)
; (3 1)
; (3 2)
; (1 4)
; done
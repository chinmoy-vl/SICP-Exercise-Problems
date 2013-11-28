(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
      							(interleave s2 (stream-cdr s1)))))


(define (interleave-streams s1 s2 s3)
  (cond ((stream-null? s1) (interleave s2 s3))
        ((stream-null? s2) (interleave s1 s3))
        ((stream-null? s3) (interleave s1 s2))
        (else (cons-stream 
               (stream-car s1)
               (cons-stream 
                (stream-car s2)
                (cons-stream 
                 (stream-car s3)
                 (interleave-streams (stream-cdr s1)
                               (stream-cdr s2)
                               (stream-cdr s3))))))))
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave-streams
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (stream-map (lambda (x) (list x (stream-car t)))
                (stream-cdr s))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (integers-from-n n)
  (cons-stream n (integers-from-n (+ 1 n))))

(define integers (integers-from-n 1))

(define int-pairs (pairs integers integers))


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
      							(interleave s2 (stream-cdr s1)))))


(define (pairs s t)
  (interleave 
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs (stream-cdr s) (stream-cdr t))))

(define (integers-from-n n)
  (cons-stream n (integers-from-n (+ 1 n))))

(define integers (integers-from-n 1))

(define int-pairs (pairs integers integers))

;results in infinite loop


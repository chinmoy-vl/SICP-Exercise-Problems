(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
      							(interleave s2 (stream-cdr s1)))))

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

(define (triples s t u)
  (cons-stream 
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s)  x))
                (pairs t (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define triplets (triples integers integers integers))

(define (pyhta-theorem-filter l)
  (= (+ (square (car l)) (square (cadr l)))
     (square (caddr l))))

(define Pytha-triplets (stream-filter pyhta-theorem-filter triplets))

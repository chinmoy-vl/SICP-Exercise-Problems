(define (weighted-pairs s t weight)
  (define (merge s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
           (let ((s1car (stream-car s1))
                 (s2car (stream-car s2)))
             (cond ((<= (weight s1car) (weight s2car))
                    (cons-stream s1car (merge (stream-cdr s1) s2)))
                   (else 
                    (cons-stream s2car (merge s1 (stream-cdr s2)))))))))
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge 
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight))))


(define (weight-a pair)
  (+ (car pair) (cadr pair)))

(define (integers-from-n n)
  (cons-stream n (integers-from-n (+ 1 n))))

(define integers (integers-from-n 1))


(define str (weighted-pairs integers integers weight-a))

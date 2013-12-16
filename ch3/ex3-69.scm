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

(define (merge-weighted proc s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
            (let ((s1car (stream-car s1))
                 (s2car (stream-car s2)))
            (if (proc s1car s2car)
                (cons-stream s1car 
                             (merge-weighted proc (stream-cdr s1) s2))
                (cons-stream s2car 
                             (merge-weighted proc s1 (stream-cdr s2))))))))

(define (weight p1 p2)
  (<  (sum-squared p1)
      (sum-squared p2)))

(define (sum-squared p1)
  (+ (square (car p1))
      (square (cadr p1))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave 
      (stream-map (lambda (x) (list (stream-car s) x))
                t)
      (pairs (stream-cdr s) (stream-cdr t)))))


(define (triples s t u)
  (define triples-iter
    (cons-stream
     (list (stream-car s) (stream-car t) (stream-car u))
     (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
                  (pairs (stream-cdr t) (stream-cdr u)))
      (triples-iter (stream-cdr s) (stream-cdr t) (stream-cdr u))))))



;(define (triples s t u)
;  (cons-stream
;   (list (stream-car s) (stream-car t) (stream-car u))
;   (interleave
;    (stream-map (lambda (x) (list (stream-car s) (stream-car (stream-cdr t)) x))
;                (stream-cdr u))
;    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))


(define (generate-stream stream n)
  (define (generate-stream-iter stream start limit)
    (cond ((< start limit ) (display (stream-car stream)) 
                    (newline) 
                    (generate-stream-iter (stream-cdr stream) (+ start 1) limit))
        (else (display "done"))))
  (generate-stream-iter stream 0 n))


(define (pyhtagorean triplet)
  (=  (+ (square (car triplet))
         (square (cadr triplet)))
      (square (caddr triplet))))

(define (test-filter triplet)
  (=  (+  (car triplet)
         (cadr triplet))
      (caddr triplet)))

(define (first-n n)
  (generate-stream (stream-filter pyhtagorean (triples integers integers integers) ) n))
  ;(generate-stream (triples integers integers integers) n))



(define (count-elements-before-pair pair-car pair-cadr pair-caddr stream)  
  (define (iterate pair-car pair-cadr stream count)
    (if (> pair-car pair-cadr)
        (display "Invalid Pair(i,j), i must be <=  j ")
        (cond ((and (=  pair-car (car (stream-car stream)))
                    (=  pair-cadr (cadr (stream-car stream)))
                    (=  pair-caddr (caddr (stream-car stream)))) 
              (display "Count: ")
              (display (car count))
              (display (stream-car stream)))
              (else
                (iterate pair-car pair-cadr (stream-cdr stream) (stream-cdr count))))))
  (iterate pair-car pair-cadr stream integers))
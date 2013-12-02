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

(define (test-stream s t)
  (interleave
             (stream-map (lambda (x) (list (stream-car s) x))
                         (stream-cdr t))
             (stream-map (lambda (x) (list x (stream-car t)))
                         (stream-cdr s))))

(define (is-equal? p1 p2)
  (and (= (car p1) (car p1))
       (= (cadr p1) (cadr p2))))


(define (find-dup s)
  (stream-map (lambda (x) 
                      ;(display (cadr x))
                      (if (is-equal? (stream-car s) x)
                          (begin  (display "duplicate found")
                                  (newline)
                                  (display x))
                          (display "")))
              (stream-cdr s)) 
  (find-dup (stream-cdr s)))


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




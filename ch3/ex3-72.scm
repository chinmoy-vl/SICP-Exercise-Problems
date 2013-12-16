(define ones
  (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers 
  (cons-stream 1 
                (add-streams ones integers)))


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

(define (weighted-pairs s1 s2)
  (cons-stream (list (stream-car s1) (stream-car s2))
                (merge-weighted
                  weigh-pairs
                  (stream-map (lambda (x) (list (stream-car s1) x)) (stream-cdr s2))
                  (weighted-pairs (stream-cdr s1) (stream-cdr s2)))))


(define (check-squared-sum stream)
  (let ((p1 (stream-car stream))
        (p2 (stream-car (stream-cdr stream)))
        (p3 (stream-car (stream-cdr (stream-cdr stream)))))
        ;(display (squared-sum p1))
        ;(newline)
        ;(display (squared-sum p2))
  (cond ((= (squared-sum p1)
            (squared-sum p2)
            (squared-sum p3))
          (cons-stream (list p1 p2 p3)
                       (check-squared-sum (stream-cdr (stream-cdr (stream-cdr stream))))))
                       ;(check-squared-sum (stream-cdr stream))))
        (else 
            (check-squared-sum (stream-cdr stream))))))


(define (squared-sum pair)
  (+ (square (car pair)) (square (cadr pair))))


(define (weigh-pairs p1 p2)
  (<= (squared-sum p1) (squared-sum p2)))

(define (pair-weight pair)
  (+ (car pair) (cadr pair)))  


(define (generate-stream stream n)
  (define (generate-stream-iter stream start limit)
    (cond ((< start limit ) 

                    (display (car (stream-car stream)))
                    (display (cadr (stream-car stream)))
                    (display (caddr (stream-car stream)))
                    (display " : ")
                    (display (squared-sum (car (stream-car stream))))
                    (newline) 

                    (generate-stream-iter 
                      (stream-cdr (stream-cdr (stream-cdr stream))) 
                      (+ start 1) 
                      limit))
        (else (display "done"))))
  (generate-stream-iter stream 0 n))


(define (first-n n)
  (generate-stream (check-squared-sum (weighted-pairs integers integers)) n))


; (first-n 10)
; (1 18)(6 17)(10 15) : 325
; (7 26)(10 25)(14 23) : 725
; (5 30)(14 27)(21 22) : 925
; (5 35)(17 31)(25 25) : 1250
; (1 38)(17 34)(22 31) : 1445
; (5 40)(16 37)(20 35) : 1625
; (12 41)(15 40)(23 36) : 1825
; (5 45)(23 39)(31 33) : 2050
; (4 47)(17 44)(25 40) : 2225
; (8 49)(16 47)(23 44) : 2465
; done




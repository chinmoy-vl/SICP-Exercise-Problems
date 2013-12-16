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



(define (weigh-pairs p1 p2)
  (<= (pair-weight p1) (pair-weight p2)))

(define (pair-weight pair)
  (let ((i (car pair))
        (j (cadr pair)))
  (+ (* 2 i) (* 3 j) (* 5 i j))))

(define (divisiblity-test pair)
  (define (div-test x)
    (not (or 
              (= (remainder x 2) 0)
              (= (remainder x 3) 0)
              (= (remainder x 5) 0))))
  (and  (div-test (car pair))
        (div-test (cadr pair))))


(define (generate-stream stream n)
  (define (generate-stream-iter stream start limit)
    (cond ((< start limit ) 
                    (display (stream-car stream)) 
                    ;(display " : ")
                    ;(display (pair-weight (stream-car stream)))
                    (newline) 
                    (generate-stream-iter (stream-cdr stream) (+ start 1) limit))
        (else (display "done"))))
  (generate-stream-iter stream 0 n))


(define (first-n n)
  (generate-stream (stream-filter divisiblity-test (weighted-pairs integers integers)) n))

;(first-n 10)
;(1 1)
;(1 7)
;(1 11)
;(1 13)
;(1 17)
;(1 19)
;(1 23)
;(1 29)
;(1 31)
;(7 7)
;done
;Unspecified return value
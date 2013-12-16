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


(define (ramanujans stream)
  (let ((p1 (stream-car stream))
        (p2 (stream-car (stream-cdr stream))))
        ;(display (cubed-sum p1))
        ;(newline)
        ;(display (cubed-sum p2))
  (cond ((= (cubed-sum p1)
            (cubed-sum p2))
          (cons-stream p1
                       (ramanujans (stream-cdr (stream-cdr stream)))))
          
        (else 
            (ramanujans (stream-cdr stream))))))


(define (cubed-sum pair)
  (+ (cube (car pair)) (cube (cadr pair))))

(define (cube x)
  (* x x x))

(define (weigh-pairs p1 p2)
  (<= (cubed-sum p1) (cubed-sum p2)))

(define (pair-weight pair)
  (+ (car pair) (cadr pair)))  


(define (generate-stream stream n)
  (define (generate-stream-iter stream start limit)
    (cond ((< start limit ) 

                    (display (cubed-sum (stream-car stream))) 
                    (newline) 
                    (generate-stream-iter (stream-cdr stream) (+ start 1) limit))
        (else (display "done"))))
  (generate-stream-iter stream 0 n))


(define (first-n n)
  (generate-stream (ramanujans (weighted-pairs integers integers)) n))


; (first-n 10)
; 1729
; 4104
; 13832
; 20683
; 32832
; 39312
; 40033
; 46683
; 64232
; 65728
; done


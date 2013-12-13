(load "common-stream-proc.scm")

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (cons-stream 1.0
                (stream-map (lambda (guess)
                              (sqrt-improve guess x))
                            (sqrt-stream x))))

(define (stream-limit stream tolerance)
  (if (<
        (abs 
          (- 
            (stream-car stream)
            (stream-car (stream-cdr stream))))
        tolerance)
      (stream-car (stream-cdr stream))
      (stream-limit (stream-cdr stream) tolerance)))


(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (partial-sums s)
  (cons-stream (stream-car s) 
                (add-streams (partial-sums s) (stream-cdr s))))

(define (ln-summands n)
  (cons-stream (/ 1.0 n)
                (stream-map - (ln-summands (+ n 1)))))  

(define ln2-stream
  (partial-sums (ln-summands 1)))

(newline)
(show-series
  (accelerated-sequence 
    euler-transform
    ln2-stream)
  3)


;output
;1.
;.7
;.6932773109243697
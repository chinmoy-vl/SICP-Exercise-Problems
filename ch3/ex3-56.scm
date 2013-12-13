(load "common-stream-proc.scm")

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))


(define (scale-stream stream factor)
  (stream-map
    (lambda (x) (* factor x))
    stream))  


(define s
  (cons-stream 1 (merge
                    (scale-stream s 2)
                    (merge
                      (scale-stream s 3)
                      (scale-stream s 5)))))


;(show-series s 20)
;1
;2
;3
;4
;5
;6
;8
;9
;10
;12
;15
;16
;18
;20
;24
;25
;27
;30
;32
;36
;Value: done

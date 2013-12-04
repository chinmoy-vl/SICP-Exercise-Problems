(load "ex3-01.scm")

(define (make-monitored proc)
  (define proc-count
    (make-accumulator 0))
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) 
            (proc-count 0))
          ((eq? m 'reset)
            (proc-count
              (*  -1
                  (proc-count 0))))
          (else 
              (proc-count 1)
              (proc m))))
  dispatch)

(define s (make-monitored sqrt))

;(s 'how-many-calls?)
;Value: 0

;(s 100)
;Value: 10

;(s 'how-many-calls?)
;Value: 1

;(s 25)
;Value: 5

;(s 'how-many-calls?)
;Value: 2

;(s 'reset)
;Value: 0

;(s 20)
;Value: 4.47213595499958

;(s 'how-many-calls?)
;Value: 1

;(s 25)
;Value: 5

;(s 'how-many-calls?)
;Value: 2

(define t (make-monitored square))
;Value: t

;(t 2)
;Value: 4

;(t 'how-many-calls?)
;Value: 1

;(t 10)
;Value: 100

;(s 'how-many-calls?)
;Value: 2

;(t 'reset)
;Value: 0

;(t 'how-many-calls?)
;Value: 0






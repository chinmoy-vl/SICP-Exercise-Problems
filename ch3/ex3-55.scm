(load "common-stream-proc.scm")


(define (integers-from-n n)
  (cons-stream n (add-streams 
                    ones 
                    (integers-from-n n)))) 

(define (partial-sums s)
  (cons-stream (stream-car s) 
                (add-streams (partial-sums s) (stream-cdr s))))


;(show-series (partial-sums integers) 10)
;1
;3
;6
;10
;15
;21
;28
;36
;45
;55

(load "common-stream-proc.scm")

(define (expand num den radix)
  (cons-stream 
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

;(define x (expand 1 7 10))
;(show-series x 10)
;1
;4
;2
;8
;5
;7
;1
;4
;2
;8
;Value: done

;(define z (expand 3 8 10))
;(show-series z 10)
;3
;7
;5
;0
;0
;0
;0
;0
;0
;0
;Value: done

(load "common-stream-proc.scm")


(define (integers-from-n n)
  (cons-stream n (add-streams 
                    ones 
                    (integers-from-n n)))) 

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams factorials (integers-from-n 2))))


;(show-series factorials 10)
;1
;2
;6
;24
;120
;720
;5040
;40320
;362880
;3628800
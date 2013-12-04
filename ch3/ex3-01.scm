(define (make-accumulator amt)
  (lambda (x)
    (begin  
      (set! amt (+ x amt))
      amt)))

(define acc (make-accumulator 100))

(define acc2 (make-accumulator 100))


;(acc 20)                           
;Value: 120

;(acc 20.5)
;Value: 140.5

;(acc2 -90)
;Value: 10

;(acc2 40)
;Value: 50

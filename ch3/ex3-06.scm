(define random-init 79)

;(define rand
;  (let ((x random-init))
;    (lambda ()
;      (set! x (rand-update x))
;      x)))

(define (rand-update x)
  (let ((a 101)
        (m 13)
        (b 97))
  (modulo 
    (+
      (* a x)
      b)
    m)))

(define rand
  (let ((x random-init))
  (define (dispatch sym)
    (cond ((eq? sym 'generate)
            ;(lambda ()
            (begin (set! x (rand-update x))
                    x))
          ((eq? sym 'reset) 
            (lambda (a)
              (set! x a)))))
  dispatch))


;(rand 'generate)
;Value: 3

;(rand 'generate)
;Value: 10

;(rand 'generate)
;Value: 2

;(rand 'generate)
;Value: 0

;(rand 'generate)
;Value: 6

;(rand 'generate)
;Value: 1

;((rand 'reset) 3)
;Value: 1

;(rand 'generate)
;Value: 10

;(rand 'generate)
;Value: 2

;(rand 'generate)
;Value: 0

;(rand 'generate)
;Value: 6


;((rand 'reset) 0)
;Value: 6

;(rand 'generate)
;Value: 6

;(rand 'generate)
;Value: 1

;(rand 'generate)
;alue: 3
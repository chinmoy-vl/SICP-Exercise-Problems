(load "common-stream-proc.scm")

(define random-init 79)

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
  (define (dispatch sym x)
    (cond ((eq? sym 'generate)
            (rand-update x)
          ((eq? sym 'reset) 
            x)))
  (cons-stream 
    random-init
    (stream-map (rand stream) ))
  )))

  (define (rand stream x)
    (let ((symbol (stream-car stream)))
    (cond ((eq? symbol 'generate)
            (let 
              ((rand-number (rand-update x)))
              (cons-stream
                rand-number
                (rand (stream-cdr stream) rand-number))))
          ((and 
            (pair? symbol)
            (eq? (car symbol) 'reset))
              (let 
                ((rand-number 
                  (rand-update (cadr symbol))))
                (cons-stream
                  rand-number
                  (rand (stream-cdr stream) rand-number)))))))




(define s (cons-stream 'generate s))
(define t
  (cons-stream 'generate 
               (cons-stream 'generate
                            (cons-stream 
                              (list 'reset 10)
                              (cons-stream 
                                'generate 
                                (cons-stream 
                                  (list 'reset 73)
                                  (cons-stream 'generate t)))))))

(define u (rand t 0))


;(show-series u 20)
;6
;1
;2
;0
;8
;8
;8
;8
;2
;0
;8
;8
;8
;8
;2
;0
;8
;8
;8
;8
;;Value: done







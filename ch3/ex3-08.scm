(define f
  (let ((a 1))
    (define (inner-f x)
      (begin  
        (set! a (* a x))
        a))
    inner-f))

;(load "ex3-08.scm")
;(+ (f 0) (f 1))
;Value: 1

;(load "ex3-08.scm")
;(+ (f 1) (f 0))
;Value: 0
 
;(load "ex3-08.scm")
;(f 1)
;Value: 1

;(f 0)
;Value: 0

;(f 1)
;Value: 0


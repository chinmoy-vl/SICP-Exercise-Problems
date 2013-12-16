(load "common-stream-proc.scm")

(define (random-numbers-in-range low high init)
  (define random-max 15000)
  (define random-numbers
    (cons-stream init
                 (stream-map rand-update random-numbers)))
  (define (rand-update x)
    (let ((a 1024)
          (c 1103)
          (m 11123))
      (modulo 
        (+ 
          (* a x)
          c)
        m)))
  (let ((range (- high low)))
    (stream-map (lambda (x) 
                  (+ low (* range (/ x random-max))))
                random-numbers)))


(define (monte-carlo experiment-stream trails-passed trails-failed)
  (define (iter trails-passed trails-failed)
    (cons-stream
     (/ trails-passed 
        (+ trails-passed trails-failed))
     (monte-carlo
      (stream-cdr experiment-stream)
      trails-passed
      trails-failed)))
  (if (stream-car experiment-stream)
      (iter 
        (+ trails-passed 1)
        trails-failed)
      (iter 
        trails-passed 
        (+ trails-failed 1))))

(define (estimate-integral P x1 x2 y1 y2)
  (define ex-stream
    (stream-map (lambda (x y) (P x y))
                (random-numbers-in-range x1 x2 100) 
                (random-numbers-in-range y1 y2 800)))
  (let ((rect-area (* (- x2 x1) (- y2 y1))))
    (stream-map (lambda (frac)
                  (* frac rect-area))
                (monte-carlo ex-stream 0 0))))

(define (unit-circle-area x y)
  (< (+ (square x) (square y)) 1.0))

(define pi-stream
  (estimate-integral unit-circle-area
                     -1.0 1.0 -1.0 1.0))


;(show-series pi-stream 10)
;0
;2.
;2.6666666666666665
;3.
;3.2
;3.3333333333333335
;3.4285714285714284
;3.5
;3.5555555555555554
;3.6
;;Value: done
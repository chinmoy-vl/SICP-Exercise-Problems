(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))  

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))



(define (estimate-integral predicate x1 x2 y1 y2 trials)  
  ;exp returns #t or #f if depending on the predicate
  (define (experiment)
    (let  ((x (random-in-range (min x1 x2) (max x1 x2)))
          (y (random-in-range (min y1 y2) (max y1 y2))))
      ;(newline)
      ;(display x)
      ;(newline)
      ;(display y)
      (predicate x y)))
  (let ((area-rect 
          ( *
            (abs (- x1 x2))
            (abs (- y1 y2))))
          (monte-carlo-fraction 
            (monte-carlo trials experiment)))
    ;(newline)
    ;(display area-rect)
    ;(newline)
    ;(display monte-carlo-fraction)
    (/ 
      ( *
        area-rect
        monte-carlo-fraction)
      1.0)))


(define (predicate-circle a b radius)
  (lambda (x y)
    (<= 
      (+
        (square (- x a))
        (square (- y b)))
      (square radius))))

(define unit-cicle
  (predicate-circle 0 0 1))


;(define pi1 
;  (estimate-integral unit-cicle -1.0 1.0 -1.0 1.0 100))


;(define pi2
;  (estimate-integral unit-cicle -1.0 1.0 -1.0 1.0 1000))

(define (pi n)
  (estimate-integral unit-cicle -1 1 1 -1 n))




;(pi 10)
;Value: 3.6

;(pi 1)
;Value: 0

;(pi 100)
;Value: 3.

;(pi 1000)
;Value: 2.984

;(pi 1000)
;Value: 3.028

;(pi 1000)
;Value: 3.016

;(pi 10000)
;Value: 3.0228

;(pi 10000)
;Value: 2.9944

;(pi 10000)
;Value: 3.0092

;(pi 100000)
;Value: 2.99612

;(pi 100000)
;Value: 3.00536

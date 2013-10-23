(define tolerance 0.0001)

(define (iterative-improve good-enough? improve)
   (define (iterate guess)
     (if (good-enough? guess)
         guess
         (iterate (improve guess))))
   iterate)



(define (average x y)
  (/ (+ x y) 2))

;sqrt in terms of iterative improve-guess
(define (sqrt x)
   ((iterative-improve (lambda (guess)
                         (< (abs (- (square guess) x))
                            tolerance))
                       (lambda (guess)
                         (average guess (/ x guess))))
   1.0))



;fixed-point in terms of iterative improve-guess
(define (fixed-point f first-guess)
   ((iterative-improve (lambda (guess)
                         (< (abs (- (f guess) guess))
                            tolerance))
                       (lambda (guess)
                         (f guess)))
    first-guess))

(define (sqrt-fixed-point x)
	(fixed-point (lambda (y) (average y (/ x y)))
								1.0))

;((iterative-improve good-enough? try) 25)
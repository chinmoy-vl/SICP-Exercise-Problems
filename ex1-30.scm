(define (sum term a next b)
 (define (iterate a result)
   (if (> a b)
       result
       (iterate (next a) (+ (term a) result))))
 (iterate a 0))
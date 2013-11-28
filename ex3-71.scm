;WIP, error while running

(define (weighted-pairs s t weight)
  (define (merge s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
           (let ((s1car (stream-car s1))
                 (s2car (stream-car s2)))
             (cond ((<= (weight s1car) (weight s2car))
                    (cons-stream s1car (merge (stream-cdr s1) s2)))
                   (else 
                    (cons-stream s2car (merge s1 (stream-cdr s2)))))))))
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge 
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight))))


(define (cube x)
  (* x x x))

(define (sum-cube pair)
  (+ (cube (car pair)) (cube (cadr pair))))

(define (integers-from-n n)
  (cons-stream n (integers-from-n (+ 1 n))))

(define integers (integers-from-n 1))

;(define str (weighted-pairs integers integers weight-a))

(define (ramanujan-number)
  (define (ramanujans stream)
    (let ((curr (stream-car stream)))
    			(next  (stream-car (stream-cdr stream)))
    			((cubed-curr (sum-cube curr)))
    			(cond ((= cubed-curr (sum-cube next)) 
    								(cons-stream (list cubed-curr curr next)
    														 (ramanujans (stream-cdr (stream-cdr stream)))))
    			      (else	(ramanujans (stream-cdr stream))))))
	(ramanujans (weighted-pairs integers integers sum-cube)))


(load "common-stream-proc.scm")


(define (display-stream-pair p num)
  (define (internal index)
    (if (> index num) 'done
        (begin
          (display
            (list (stream-ref (car p) index)
                  (stream-ref (cdr p) index)))
          (newline)
          (internal (+ 1 index)))))
  (newline)
  (internal 0))
  


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (RLC R L C dt)
  (lambda(vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1 C)))
    (define dil (add-streams (scale-stream il (/ (* -1 R) L))
                             (scale-stream vc (/ 1 L))))
    (cons vc il)))

(define RLC123 (RLC 1 0.2 1 0.1))

;(display-stream-pair (RLC123 10 0) 40)

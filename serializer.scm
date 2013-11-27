(define (make-serializer)
  (let ((mutex (make-mmutex)))
  	(lambda (p)
  		(define (serilized-p . args)
  		  (mutex 'accquire)
  			(let ((val (apply p args)))
  				(mutex 'release)
  				val))
  	serilized-p)))

(define (make-mmutex)
  (let ((cell (list false)))
  	(define (the-mutex m)
  		(cond ((eq? m 'accquire)
  					 (if (test-and-set! cell)
  					     (the-mutex 'accquire)
  					     ((eq? m 'release) (clear! cell))))))
  	the-mutex))


(define (clear! cell)
	(set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
      				false)))
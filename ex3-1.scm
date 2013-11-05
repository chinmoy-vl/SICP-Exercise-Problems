(define (make-accumulator sum)
  (define (accumulator val)
    (begin (set! sum (+ sum val)) sum))
	(define dispatch
	  accumulator)
	dispatch)
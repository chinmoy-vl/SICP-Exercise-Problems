(define (make-queue)
	(let ((front-ptr '())
				(rear-ptr '()))
	(define (set-front-ptr! item) (set! front-ptr item))
	(define (set-rear-ptr! item) (set! rear-ptr item))
	(define (empty-q?)  (null? front-ptr))

	(define (front-q queue)
  	(if (empty-q?)
      	(error "FRONT called with an empty queue")
      	(car front-ptr)))

	(define (insert-q! item)
	  (let ((new-pair (cons item '())))
	  	(cond ((empty-q?)
	  				(set-front-ptr! new-pair)
	  				(set-rear-ptr! new-pair))		
	  	      (else 
		      			(set-cdr! front-ptr new-pair)
		      			(set! rear-ptr new-pair)))))

	(define (delete-q! queue)
		(cond ( (empty-q?)	
						(error "DELELTE! called with an empty queue"))		
		      (else 
		      		(set-front-ptr! (cdr (front-q queue))))))

	(define print-q
	  (display "front-ptr"))

	(define (dispatch m)
	  (cond ((eq? m 'insert-queue!) insert-q!)
	        ((eq? m 'delete-queue!) delete-q!)
	        ((eq? m 'empty-queue?) empty-q!)
	      	;((eq? m 'front-queue) front-q)
	      	((eq? m 'set-front-ptr!) set-front-ptr!)
	      	((eq? m 'set-rear-ptr!) set-rear-ptr!)
	      	((eq? m 'print-queue) print-q)))
	dispatch))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item)
  queue)
(define (delete-queue! queue)
  (queue 'delete-queue!)
  queue)
(define (empty-queue? queue)
  (queue 'empty-queue?))
;(define (front-queue queue)
;  (queue 'front-queue))
(define (print-queue queue)
  (queue 'print-queue))

;(define q (make-queue))

;(print-queue (insert-queue! q1 'a))
;(print-queue (insert-queue! q1 'b))
;(print-queue (delete-queue! q1))
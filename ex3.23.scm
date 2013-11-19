(define (make-deque)
	(let ((front-ptr '())
				(rear-ptr '()))
	(define (set-front-ptr! item) (set! front-ptr item))
	(define (set-rear-ptr! item) (set! rear-ptr item))
	(define (empty-deque?)  (null? front-ptr))

	(define (front-dq deque)
  	(if (empty-deque?)
      	(error "FRONT called with an empty deque")
      	(car front-ptr)))

	(define (rear-dq deque)
  	(if (empty-deque?)
      	(error "REAR called with an empty deque")
      	(cdr rear-ptr)))

	(define (front-insert-dq! item)
	  (let ((new-pair (cons item '())))
	  	(cond ((empty-deque?)
	  				(set-front-ptr! new-pair)
	  				(set-rear-ptr! new-pair))		
	  	      (else 
		      			(set-cdr! new-pair front-ptr)
		      			(set! front-ptr new-pair)))))

	(define (rear-insert-dq! item)
	  (let ((new-pair (cons item '())))
	  	(cond ((empty-deque?)
	  				(set-front-ptr! new-pair)
	  				(set-rear-ptr! new-pair))		
	  	      (else 
		      			(set-cdr! (cdr rear-ptr) new-pair)
		      			(set! rear-ptr new-pair)))))

	(define (front-delete-dq! deque)
		(cond ( (empty-deque?)	
						(error "DELELTE! called with an empty deque"))		
		      (else 
		      		(set-front-ptr! (cdr (front-deque deque))))))

	(define (rear-delete-dq! deque)
		(cond ( (empty-deque?)	
						(error "DELELTE! called with an empty deque"))		
		      (else 
		      		(set-front-ptr! (cdr (front-deque deque))))))

	(define print-dq
	  (display front-ptr))

	(define (dispatch m)
	  (cond ((eq? m 'front-insert-deque!) front-insert-dq!)
	  			((eq? m 'rear-insert-deque!) rear-insert-dq!)
	        ((eq? m 'front-delete-deque) front-delete-dq!)
	        ((eq? m 'rear-delete-deque) rear-delete-dq!)
	      	((eq? m 'front-deque) front-dq)
	      	((eq? m 'rear-deque) rear-dq)
	      	((eq? m 'set-front-ptr!) set-front-ptr!)
	      	((eq? m 'set-rear-ptr!) set-rear-ptr!)
	      	((eq? m 'print-deque) print-dq)))
	dispatch))

(define (front-insert-deque! deque item)
  ((deque 'front-insert-deque!) item)
  deque)
(define (rear-insert-deque! deque item)
  ((deque 'rear-insert-deque!) item)
  deque)

(define (front-delete-deque! deque)
  (deque 'front-delete-deque!)
  deque)
(define (rear-delete-deque! deque)
  (deque 'rear-delete-deque!)
  deque)

(define (empty-deque? deque)
  (deque 'empty-deque?))
(define (front-deque deque)
  (deque 'front-deque))
(define (rear-deque deque)
  (deque 'rear-deque))
(define (print-deque deque)
  (deque 'print-deque))

(define dq (make-deque))

;(print-queue (insert-queue! q1 'a))
;(print-queue (insert-queue! q1 'b))
;(print-queue (delete-queue! q1))
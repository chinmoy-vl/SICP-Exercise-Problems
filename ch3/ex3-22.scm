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

  (define (delete-q!)
    (cond ( (empty-q?)  
            (error "DELELTE! called with an empty queue"))    
          (else 
              (set-front-ptr! (cdr front-ptr)))))

  (define (print-q)
    (display front-ptr))

  (define (dispatch m)
    ;(display "dispatch called")
    (cond ((eq? m 'insert-queue!) insert-q!)
          ((eq? m 'delete-queue!) delete-q!)
          ((eq? m 'empty-queue?) empty-q!)
          ((eq? m 'front-queue?) front-q!)
          ((eq? m 'print-queue) print-q)))
  dispatch))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))
(define (delete-queue! queue)
  ((queue 'delete-queue!)))
(define (print-queue queue)
  ((queue 'print-queue)))


;(define q (make-queue))
;Value: q

;(insert-queue! q 2)
;Value: ()

;(print-queue q)
;(2)

;(insert-queue! q 5)
;Value 21: (2 5)

;(print-queue q)
;(2 5)

;(delete-queue! q)
;Value 27: (2 5)

;(print-queue q)
;(5)

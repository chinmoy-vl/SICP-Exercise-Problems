(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (make-deque) (cons '() '()))
(define (empty-deque? deque) 
  (and (null? (front-ptr deque))
       (null? (rear-ptr deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-car! (cdr new-pair) (rear-ptr deque))
           (set-cdr! (cdr (rear-ptr deque)) new-pair)
           (set-rear-ptr! deque new-pair)))))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-cdr! (cdr new-pair) (front-ptr deque))
           (set-car! (cdr (front-ptr deque)) new-pair)
           (set-front-ptr! deque new-pair)))))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-front-ptr! deque (cddr (front-ptr deque)))
         (set-car! (cdr (front-ptr deque)) '()))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-rear-ptr! deque (cadr (rear-ptr deque)))
         (set-cdr! (cdr (rear-ptr deque)) '()))))

(define (print-deque deque)
  (define (print-dq q)
    (if (null? q)
        '()
        (cons (car q) 
              (print-dq (cddr q)))))
  (newline)
  (display (print-dq (front-ptr deque))))


;(define dq (make-deque))
;Value: dq

;(front-insert-deque! dq 'a)

;(print-deque dq)
;(a)

;(front-insert-deque! dq 'b)

;(print-deque dq)
;(b a)

;(rear-insert-deque! dq 'x)

;(print-deque dq)
;(b a x)

;(rear-insert-deque! dq 'x)

;(print-deque dq)
;(b a x x)

;(rear-delete-deque! dq)

;(print-deque dq)
;(b a x)

;(front-delete-deque! dq)
;(front-delete-deque! dq)

;(print-deque dq)
;(x)

;(rear-delete-deque! dq)

;(rear-delete-deque! dq)
;REAR-DELETE! called with an empty deque (())
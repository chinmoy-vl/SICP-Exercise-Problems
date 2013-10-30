
(define (equal? a b)
  (cond ((and (null? b) (null? a)) #t)
  			((or (null? b) (null? a)) #f)
        ((and (pair? a) (pair? b)) (and (equal? (car a) (car b))
        																(equal? (cdr a) (cdr b))))
        ((or (pair? a) (pair? b)) #f)
      	(else (eq? a b))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
				(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set) 
                                (adjoin-set x 
                                            (cdr set))))))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
          (cons (car set1)
                (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))
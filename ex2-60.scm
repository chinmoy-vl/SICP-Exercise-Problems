
(define (equal? a b)
  (cond ((and (null? b) (null? a)) #t)
  			((or (null? b) (null? a)) #f)
        ((and (pair? a) (pair? b)) (and (equal? (car a) (car b))
        																(equal? (cdr a) (cdr b))))
        ((or (pair? a) (pair? b)) #f)
      	(else (eq? a b))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
				(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  ;(if (element-of-set? x set)
  ;    set
      (cons x set))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
          (cons (car set1)
                (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))
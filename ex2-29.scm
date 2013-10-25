

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length mobile)
  (car mobile))

(define (branch-structure mobile)
  (car (cdr mobile)))

(define (total-weight mobile)
	((+ (branch-weight (left-branch mobile))
			(branch-weight (right-branch mobile)))))
  ;(cond ((null? mobile) 0)
  ;      ((not (pair? mobile)) mobile)
		;		(else (+	(total-weight (left-branch mobile))
		;							(total-weight (right-branch mobile))))))

(define (branch-weight branch)
  (cond ((null?  branch) 0)
        ((not (pair? (branch-structure branch))) (branch-structure branch))
      	(else (branch-weight (branch-structure branch)))))

(define (test-mobile)
  (make-mobile	(make-branch 2 
  													(make-mobile 	(make-branch 3 5)
        																	(make-branch 5 2)))
								(make-branch 10 8)))

;(display (left-branch test-mobile))
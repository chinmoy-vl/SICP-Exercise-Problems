(define (or-gate a1 a2 output)
  (define (and-action-preocedure)
    (let ((new-value
    			(logical-or (get-signal a1) (get-signal a2))))	
    	(after-delay or-gate-delay
    								(lambda ()
    													(set-signal! output new-value)))))
	(add-action! a1 and-action-preocedure)
	(add-action! a2 and-action-preocedure))

(define (logical-or a b)
  (cond ((or (= a 1) (= b 1)) 1)
        (and (= a 0) (= b 0) 0)
      	(else (error "invalid signal"))))
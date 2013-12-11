(define (or-gate a1 a2 output)
  (define (or-action-preocedure)
    (let ((new-value
    			(logical-or (get-signal a1) (get-signal a2))))	
    	(after-delay or-gate-delay
    								(lambda ()
    													(set-signal! output new-value)))))
	(add-action! a1 or-action-preocedure)
	(add-action! a2 or-action-preocedure))

(define (logical-or a b)
  (cond ((or (= a 1) (= b 1)) 1)
        (and (= a 0) (= b 0) 0)
      	(else (error "invalid signal"))))
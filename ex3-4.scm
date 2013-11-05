


(define (make-account balance password)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient balance"))
	
	(define (deposit amount)
	  (set! balance (+ balance amount)) balance)

	(define counter-show-error 0)

	(define reset-counter
	  (set! counter-show-error 0))

	(define call-the-cops
	  "Cops are called")

	(define (show-error-msg amt)
		(if (> counter-show-error 3)
		    call-the-cops
		    "Incorrect password"))

	(define (dispatch passwd m)
	      (if (not (eq? password passwd))
	          (begin (set! counter-show-error (+ counter-show-error 1)) show-error-msg)
    				(begin (set! counter-show-error 0)
    								(cond ((eq? m 'withdraw)  withdraw)
				    				  		((eq? m 'deposit) deposit)
				    				      (else (error "unknown request -- MAKE_ACCOUNT" m))))))
	dispatch)

(define acc (make-account 100 'secret-password))
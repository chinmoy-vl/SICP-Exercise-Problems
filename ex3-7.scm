


(define (make-account balance password)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient balance"))
	
	(define (deposit amount)
	  (set! balance (+ balance amount)) balance)

	(define (make-joint account old-passwd new-passwd)
	  (lambda (password m)
	    (if (eq? password new-passwd)
	        (account old-passwd m)
	        "Incorrect password")))

	(define (show-error-msg amt)
	  "Incorrect password")

	(define (dispatch passwd m)
	      (cond ((not (eq? password passwd)) show-error-msg)
			      	((eq? m 'withdraw) withdraw)
			  			((eq? m 'deposit) deposit)
			  			((eq? m 'make-joint-account) make-joint-account)
			        (else (error "unknown request -- MAKE_ACCOUNT" m))))
	dispatch)

(define peter-acc (make-account 100 'secret-password))



(define (make-account balance password)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient balance"))
	
	(define (deposit amount)
	  (set! balance (+ balance amount)) balance)

	(define (show-error-msg amt)
	  "Incorrect password")

	(define (dispatch passwd m)
	      (cond ((not (eq? password passwd)) show-error-msg)
			      	((eq? m 'withdraw) withdraw)
			  			((eq? m 'deposit) deposit)
			        (else (error "unknown request -- MAKE_ACCOUNT" m))))   
	dispatch)

(define acc (make-account 100 'secret-password))
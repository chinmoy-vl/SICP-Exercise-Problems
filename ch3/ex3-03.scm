
(define (make-account balance password)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient balance"))
  
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)

  (define (show-error amt)
    (display "Incorrect password"))

  (define (dispatch passwd m)
        (cond ((not (eq? password passwd)) show-error)
              ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "unknown request -- MAKE_ACCOUNT" m))))   
  dispatch)

(define acc (make-account 100 's-p))
(define acc2 (make-account 50 'pass))

;((acc 's-p 'withdraw) 100)
;Value: 0

;((acc 's-p 'withdraw) 100)
;Value 19: "Insufficient balance"


;((acc 's 'deposit) 30)
;Incorrect password

;((acc 's-p 'deposit) 50)
;Value: 50

;((acc2 's-p 'withdraw) 20)
;Incorrect password

;((acc2 'pass 'deposit) 30)
;Value: 80





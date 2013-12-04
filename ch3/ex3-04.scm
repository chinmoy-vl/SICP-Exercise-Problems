(load "ex3-02.scm")

(define (make-account balance password)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient balance"))
  
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)

  (define (show-error amt)
    (display "Incorrect password"))

  (define (call-the-cops amt)
    (display "call-the-cops"))

  (define monitored-error
    (make-monitored show-error))

  (define (dispatch passwd m)
        (cond 
          (   (and (> 
                      (monitored-error 'how-many-calls?)
                      2)
                    (not (eq? password passwd)))
            call-the-cops)
          ((not (eq? password passwd)) 
            monitored-error)
          ((eq? m 'withdraw)
            (monitored-error 'reset)
            withdraw)
          ((eq? m 'deposit) 
            (monitored-error 'reset)
            deposit)
          (else (error "unknown request -- MAKE_ACCOUNT" m))))
  dispatch)


(define acc3 (make-account 100 'password))

;((acc3 'passwo 'deposit) 100)                                
;Incorrect password

;((acc3 'password 'withdraw) 20)
;Value: 80

;((acc3 'passw 'withdraw) 30)   
;Incorrect password

;((acc3 'passw 'withdraw) 30)  
;Incorrect password

;((acc3 'passw 'deposit) 200)
;Incorrect password

;((acc3 'passw 'deposit) 200)
;call-the-cops

;((acc3 'passw 'deposit) 200)
;call-the-cops

;((acc3 'password 'deposit) 30)
;Value: 110

;((acc3 'p 'deposit) 10)
;Incorrect password

;((acc3 'p 'deposit) 10)
;Incorrect password

;((acc3 'p 'deposit) 10)
;Incorrect password

;((acc3 'p 'deposit) 10)
;call-the-cops

;((acc3 'password 'withdraw) 20) 
;Value: 90

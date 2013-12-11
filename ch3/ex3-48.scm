(define make-account-and-serializer
  (let ((id 0))
  (lambda (balance) 
    (set! id (+ id 1))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'get-acc-id) id)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))))


(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
          (serializer2 (account2 'serializer)))
        (if (<
              (account1 'get-acc-id)
              (account2 'get-acc-id))
                ((serializer1 
                  (serializer2 exchange))
                  account1
                  account2)
                ((serializer2 
                  (serializer1 exchange))
                  account1
                  account2)))
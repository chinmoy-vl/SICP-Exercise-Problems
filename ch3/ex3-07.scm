(load "ex3-03.scm")

  (define (make-joint acc acc-password new-password)
    (define (joint-transactions passwd m)
      (if (eq? passwd new-password)
          (acc acc-password m)
          (display "wrong password for joint account")))
    joint-transactions)


(define peter-acc (make-account 100 'open-sesame))

(define paul-acc (make-joint peter-acc 'open-sesame 'rose))

((paul-acc 'rose 'deposit) 20)
((peter-acc 'open-sesame 'deposit) 45)


;((paul-acc 'rose 'withdraw) 20)
;Value: 80

;((peter-acc 'rose 'withdraw) 20)
;Incorrect password

;((peter-acc 'open-sesame 'withdraw) 20)
;Value: 60

;((paul-acc 'rose 'deposit) 40)
;Value: 100

;((paul-acc 'open-sesame 'deposit) 20)
;wrong password for joint account

;((peter-acc 'open-sesame 'deposit) 45)
;Value: 145



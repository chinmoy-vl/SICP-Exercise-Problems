(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))


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
  (cond ((or
            (= a 1) (= b 1))
          1)
        ((and 
          (= a 0) (= b 0))
          0)
        (else (error "invalid signal"))))


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a b)
  (cond ((or (= a 0) (= b 0))
          0)
        ((and (= a 1) (= b 1))
          1)
        (else (error "invalid signal"))))
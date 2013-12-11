(define (FA Ak Bk Sk c)
  (let ((c-in (make-wire)))
    (cond
      ((null? (cdr Ak)) 
        (set-signal! c-n 0)
        (full-adder (car Ak) (car Bk) c (car sk) c-n)
      (else 
        (full-adder (car Ak) (car Bk) c (car sk) c-n)
        (FA (cdr Ak) (cdr BK) (cdr Sk) c-n))))))
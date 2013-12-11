(define (or-gate a b output)
  (let 
    ((m (make-wire)))
    (n (make-wire))
    (o (make-wire))
    (interver a m)
    (interver b n)
    (and-gate m n o)
    (interver o output)))
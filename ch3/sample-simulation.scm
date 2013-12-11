(load "agenda.scm")
(load "make-wire")
(load "logic-gates.scm")


(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define a (make-wire))
(define b (make-wire))
(define s (make-wire))
(define c (make-wire))



;(probe 'sum sum)

;sum 0  New-value = 0
;Unspecified return value

;(probe 'carry carry)

;carry 0  New-value = 0
;Unspecified return value

;(half-adder input-1 input-2 sum carry)

;Value: ok

;(set-signal! input-1 1)

;Value: done

;(propagate)

;sum 8  New-value = 1
;Value: done

;(set-signal! input-2 1)

;Value: done

;(propagate)

;carry 11  New-value = 1
;sum 16  New-value = 0
;Value: done




;(load "sample-simulation")


;(probe 'output s)

;output 0  New-value = 0
;Unspecified return value

;(inverter a s)

;Value: ok

;(set-signal! a 1)

;Value: done

;(propagate)

;output 2  New-value = 0
;Value: done

;(set-signal! a 0)

;Value: done

;(propagate)

;output 4  New-value = 1
;Value: done

;(set-signal! a 1)

;Value: done

;(propagate)

;output 6  New-value = 0
;Value: done

;(set-signal! a 0)

;Value: done

;(propagate)

;output 8  New-value = 1
;Value: done



(load "make-connector.scm")
(load "primitive-constraints.scm")
(load "celcius-f.scm")

(define C (make-connector))
(define F (make-connector))


;Value: ok

;(probe "celcius Temp" C)
;Value 21: #[compound-procedure 21 me]

;(probe "Farenheit Temp" F)
;Value 22: #[compound-procedure 22 me]

;(set-value! C 25 'user)

;Probe: celcius Temp = 25
;Probe: Farenheit Temp = 77
;Value: done

;(set-value! F 212 'user)
;Contradiction (77 212)


;(forget-value! c 'abb)
;Value: ignored

;(set-value! F 212 'user)
;Contradiction (77 212)

;(forget-value! c 'user)
;Probe: celcius Temp = ?
;Probe: Farenheit Temp = ?
;Value: done

;(set-value! F 212 'user)
;Probe: Farenheit Temp = 212
;Probe: celcius Temp = 100
;Value: done

;
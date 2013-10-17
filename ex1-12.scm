(define (pascal n)
	(pascal_counter_call 0 n))

(define (pascal_counter_call row n)
	(cond (< row n)
		(pascal_counter row n)
		(pascal_counter_call (+ row 1) n))
	)


(define (pascal_counter n count)
	(cond ((> count -1)
			(display (pascal_recur n count))
			(pascal_counter n (- count 1))) 
	))


(define (pascal_recur row col)
	(cond ((= col 0) 1)
		((= row col ) 1)
		(else (+ (pascal_recur (- row 1) (- col 1) )
						(pascal_recur (- row 1) col) ))))
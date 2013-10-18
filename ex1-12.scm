(define (pascal n)
	(pascal_counter_call 0 n))

(define (pascal_counter_call row n)
	(cond ((< row n)
			(newline)
			(show-space 0 (- n row))
			(pascal_counter row row)
			(pascal_counter_call (+ row 1) n)))
	)


(define (pascal_counter n count)
	(cond ((> count -1)
			(display (pascal_recur n count))
			(display " ")
			(pascal_counter n (- count 1))) 
	))

(define (show-space count space-count)
	(cond ((< count space-count)
				(display " ")
				(show-space (+ count 1) space-count)))
	)


(define (pascal_recur row col)
	(cond ((= col 0) 1)
		((= row col ) 1)
		(else (+ (pascal_recur (- row 1) (- col 1) )
						(pascal_recur (- row 1) col) ))))
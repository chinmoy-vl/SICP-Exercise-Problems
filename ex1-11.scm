(define (a_function_recur n)
	(cond ((< n 3)	n)
		(else (+ (a_function (- n 1))	(* 2 (a_function (- n 2))) (* 3 (a_function (- n 3))))
			)))



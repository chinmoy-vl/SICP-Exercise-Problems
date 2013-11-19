(define make-table (list '*table*))

;(define (lookup key-1 key-2 table)
;  (let ((subtable (assoc key-1 (cdr table))))
;  	(if subtable
;  	    (let ((record (assoc key-2 (cdr subtable))))
;  	    	(if record
;  	    	   (cdr record)
;  	    	 #f))
;  	  #f)))


(define (insert! keys value table)
  (let ((subtable (assoc (car keys) (cdr table))))
    (cond '(
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable))))))
          (predicate2 consequent2))
  	(if subtable
  	    (let ((record (assoc key-2 (cdr subtable))))
  	    	
  	    (set-cdr! table
  	    					(cons (list key-1 
  	    											(cons key-2 value))
  	    								(cdr table)))))
'ok)
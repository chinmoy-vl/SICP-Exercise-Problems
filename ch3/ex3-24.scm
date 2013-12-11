
(define (make-table same-key?)

  (define (check-keys key lst same-key?)
    (define (iter items)
      (cond ((null? items) #f)
            ((same-key? (caar items) key) (car items))
            (else (iter (cdr items)))))
    (iter lst))

  (let ((local-table (list '*table*)))

    (define (lookup key-1 key-2)
      (let ((subtable (check-keys key-1 (cdr local-table) same-key?)))
        (if subtable
            (let ((record (check-keys key-2 (cdr subtable) same-key?)))
              (if record
                  (cdr record)
                  #f))
            #f)))
    

    (define (insert! key-1 key-2 value)
      (let ((subtable (check-keys key-1 (cdr local-table) same-key?)))
        (if subtable
            (let ((record (check-keys key-2 (cdr subtable) same-key?)))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'done)   

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (proc k1 k2)
  (equal? k1 k2))
(define test-table (make-table proc))
(define get (test-table 'lookup-proc))
(define put (test-table 'insert-proc!))

;(put 'key1 'key2 100)
;(put 'key1 'key3 200)

;(display (get 'key1 'key2))
;100

;(display (get 'key1 'key3))
;200

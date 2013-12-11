
(define (make-table)
  (let ((local-table (list '*table*)))


    (define (find-key key otherkeys table)
      (let ((value (assoc key (cdr table))))
        (if value
          (if (null? otherkeys)
              (cons value (list))
              (if (pair? (cdr value))
                  (find-key (car otherkeys) (cdr otherkeys) value)
                  (cons value otherkeys)))
          (cons table (cons key otherkeys)))))  

    (define (lookup key-list)
      (let ((result (find-key (car key-list) (cdr key-list) local-table)))
        (let ((table (car result))
              (remaining-keys (cdr result)))
          (if (null? remaining-keys)
              (if (not (pair? (cdr table)))
                  (cdr table)
                  #f)
              #f))))

    (define (assoc key records)
      (cond ((null? records) #f)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (insert! key-list value)
      (define (insert-recur! key otherkeys value table)
        (if (null? otherkeys)
                   (set-cdr! table (cons (cons key value) (cdr table)))
            (begin (set-cdr! table (cons (cons key   '()) (cdr table)))
                   (insert-recur! (car otherkeys) (cdr otherkeys) value (cadr table)))))
      (let ((result (find-key (car key-list) (cdr key-list) local-table)))
        (let ((table (car result))
              (remaining-keys (cdr result)))
          (if (null? remaining-keys)
              (if (not (pair? (cdr table)))
                  (set-cdr! table value)
                  #f)
              (if (or (pair? (cdr table)) (null? (cdr table)))
                  (insert-recur! (car remaining-keys) (cdr remaining-keys) value table)
                  #f)))))
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'get-table) local-table)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define test-table (make-table))
(define get (test-table 'lookup-proc))
(define put (test-table 'insert-proc!))
(define get-table (test-table 'get-table))

;(put (list 'key1 'key2) 100)

;(display get-table)
;(*table* (key1 (key2 . 100)))

;(put (list 'key1 'key3 'key4) 200)

;(display get-table)
;(*table* (key1 (key3 (key4 . 200)) (key2 . 100)))

;(display (get (list 'key1 'key2)))
;100

;(display (get (list 'key1 'key3)))
;#f

;(display (get (list 'key1 'key3 'key4)))
;200

;(display (get (list 'key1 'key2 'key5)))
;#f

;(display (get (list 'key1)))
;#f

;(put (list 'key1 'key2) 1000)

;(display get-table)
;(*table* (key1 (key3 (key4 . 200)) (key2 . 1000)))

;(display (get (list 'key1 'key2)))
;1000
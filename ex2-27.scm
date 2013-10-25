

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (reverse l)
  (if (null? l)
    l
    (append (reverse (cdr l)) (list (car l)) )))

(define (deep-reverse items)
  (cond ((null? items) ())
        ((pair? (car items))
         (append (deep-reverse (cdr items))
                 (list (deep-reverse (car items)))))
        (else
         (append (deep-reverse (cdr items))
                 (list (car items))))))

(deep-reverse (list (list 1 2) (list 3 4 (list 10 11 12))))
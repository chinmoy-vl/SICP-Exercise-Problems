(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))


(define (make-code-tree left right)
  (list left
  			right
  			(append (symbols left) (symbols right))
  			(+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
      	(else (cons (car set)
      							(adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
      	(adjoin-set (make-leaf 	(car pair)
      													(cadr pair))
      							(make-leaf-set (cdr pairs))))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
  								(make-code-tree
  									(make-leaf 'B 2)
  									(make-code-tree (make-leaf 'D 1)
  																	(make-leaf 'C 1)))))


(define (memq item x)
  (cond ((null? x) #f)
        ((eq? (car x) item) x)
        (else (memq item (cdr x)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
      (cond ((not(memq symbol (symbols tree)))
              error "symbol not in tree")
            ((leaf? tree) '())
            ((memq symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree))))
            ((memq symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree))))))


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge tree)
  (if (null? (cdr tree))
      (car tree)
      (let ((node-1 (car tree))
            (node-2 (cadr tree))
            (rest (cddr tree)))
        (let ((new-node (make-code-tree node-1 node-2)))
          (successive-merge (adjoin-set new-node rest))))))



(define pairs '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

(generate-huffman-tree pairs)
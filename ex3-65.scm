(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (partial-sum stream) (cons-stream (car stream) (add-streams (partial-sum stream) (stream-cdr stream))))

(define (log-summands n)
  (cons-stream (/ 1 n)
								(stream-map -(log-summands (+ n 1)))))

(define log-stream
  (partial-sum (log-summands 1)))
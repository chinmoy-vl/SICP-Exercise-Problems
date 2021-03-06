(define (make-semaphore n)
  (let ((mutex (make-mutex)))
    (count 0)
    (define (semaphore m)
      (cond ((eq? m 'acquire)
              (if (= count n)
                  (mutex 'acquire))
              (if (< count n)
                  (set! count (+ count 1)))
            ((eq? m 'release)
              (mutex 'release)
              (if (> count 0)
                (set! count (- count 1)))))))
  semaphore))
#lang racket


; https://github.com/arclanguage/anarki/blob/master/arc.arc#L1844
(define (memo f)
  (let ((cache (make-hash)))
    (λ args
      (hash-ref cache args
                (λ ()
                  (let ((result (apply f args)))
                    (hash-set! cache args result)
                    result))))))

(define get-degree (memo (λ (vertex graph)
  (length (filter (λ (edge) (member vertex edge)) graph)))))


(module+ test
  (require rackunit)
  (check-equal? (get-degree 'a '((a z) (a e))) 2)
  (check-equal? (get-degree 'z '((a z) (a e))) 1)
  (check-equal? (get-degree 'e '((a z) (a e))) 1))


(provide get-degree)
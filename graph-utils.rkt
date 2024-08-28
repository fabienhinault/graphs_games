#lang racket


(define (deep f l)
    (cond ((null? l) '())
          ((pair? l) (cons (deep f (car l)) (deep f (cdr l))))
          (else (f l))))


(define (graph-first-node g)
  (caar g))

; second node of first edge of g
(define (graph-second-node g)
  (cadar g))

; does the graph g contains all of elements of the list l
(define (contains-all? g l)
  (cond ((null? l) true)
        ((null? g) l)
        (else (contains-all?
               (cdr g)
               (filter (λ(s) (not (or (equal? s (graph-first-node g))
                                      (equal? s (graph-second-node g)))))
                       l)))))

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


(define (get-graph-degrees graph)
  (deep (λ (_) (get-degree _ graph)) graph))

(module+ test
  (require rackunit)
  (check-equal? (get-graph-degrees '((a z) (a e))) '((2 1) (2 1))))

(define (get-graph-degree-1-vertices graph vertices)
  (filter (λ (_) (equal? 1 (get-degree _ graph)))
          vertices))



(module+ test
  (check-equal? (get-degree 'a '((a z) (a e))) 2)
  (check-equal? (get-degree 'z '((a z) (a e))) 1)
  (check-equal? (get-degree 'e '((a z) (a e))) 1))

(define (tailrec-graph->node-set graph nodes-set)
  (if (null? graph)
      nodes-set
      (tailrec-graph->node-set
       (cdr graph)
       (set-add (set-add nodes-set (graph-first-node graph)) (graph-second-node graph)))))

(define (graph->node-set graph)
  (tailrec-graph->node-set graph (set)))



(provide
 deep
 get-degree
 get-graph-degrees
 contains-all?
 graph->node-set graph-first-node graph-second-node get-graph-degree-1-vertices)
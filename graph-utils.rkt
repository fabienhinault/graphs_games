#lang racket



(define (replace-all-deep old new l)
  (cond ((null? l) '())
        ((equal? l old) new)
        ((pair? l) (cons (replace-all-deep old new (car l)) (replace-all-deep old new (cdr l))))
        (else l)))

(module+ test
  (require rackunit)
  (check-equal? (replace-all-deep 'a 'b '()) '())
  (check-equal? (replace-all-deep 'a 'b 'a) 'b)
  (check-equal? (replace-all-deep 'a 'b 'b) 'b)
  (check-equal? (replace-all-deep 'a 'b 'z) 'z)
  (check-equal? (replace-all-deep 'a 'b '((a z) (e r))) '((b z) (e r))))


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


; nodes-so-far-set = vertices reached from the first one
; unused-edges = edges that do not connect to the vertices of nodes-so-far-set
(define (tailrec-is-graph-connected? graph all-nodes-set nodes-so-far-set unused-edges)
  (if (null? graph)
      (if (null? unused-edges)
          (set=? all-nodes-set nodes-so-far-set)
          (if (set-empty? (set-intersect (graph->node-set unused-edges) nodes-so-far-set))
              ; not connected. propose an edge to add.
              (list (graph-first-node unused-edges) (set-first nodes-so-far-set))
              (tailrec-is-graph-connected? unused-edges all-nodes-set nodes-so-far-set '())))
      (cond ((set-member? nodes-so-far-set (graph-first-node graph))
             ; if the first vertex of the remaining edges is in the nodes reached,
             ; then add the second vertex
             (tailrec-is-graph-connected?
              (cdr graph)
              all-nodes-set
              (set-add nodes-so-far-set (graph-second-node graph))
              unused-edges))
            ((set-member? nodes-so-far-set (graph-second-node graph))
             ; if not, but the second vertex is, then add the first one
             (tailrec-is-graph-connected?
              (cdr graph)
              all-nodes-set
              (set-add nodes-so-far-set (graph-first-node graph))
              unused-edges))
            (else
             ; if none are, add the first edge to unused-edges
             (tailrec-is-graph-connected?
              (cdr graph)
              all-nodes-set
              nodes-so-far-set
              (cons (car graph) unused-edges))))))

(module+ test
  (define set-abcd  (set 'a 'b 'c 'd))
  (check-true (tailrec-is-graph-connected? '((a b) (c d) (a c))
                                           (set 'a 'b 'c 'd)
                                           (set 'a)
                                           '()))
  (check-true (tailrec-is-graph-connected? '((a b) (a d) (a c))
                                           (set 'a 'b 'c 'd)
                                           (set 'a)
                                           '()))
  (check-not-equal? true (tailrec-is-graph-connected? '((a b) (c d)) (set 'a 'b 'c 'd) (set 'a) '()))
  (check-not-equal? true (tailrec-is-graph-connected? '((c d)) (set 'a 'b 'c 'd) (set 'a ' b) '()))
  (check-not-equal? true (tailrec-is-graph-connected? '() (set 'a 'b 'c 'd) (set 'a ' b) '((c d))))
  (check-true (tailrec-is-graph-connected? '((a b) (b c)) (set 'a 'b 'c) (set 'a) '()))
  (check-true (tailrec-is-graph-connected? '((a b) (a c)) (set 'a 'b 'c) (set 'a) '()))
  (check-true (tailrec-is-graph-connected? '((a b)) (set 'a 'b) (set 'a) '()))
  (check-true (tailrec-is-graph-connected? '() (set) (set) '())))

; auxiliary function for get-graph-nodes-by-degrees
(define (add-node-by-degree acc v degree)
  (let ((all-nodes (vector-ref acc 0))
        (deg-nodes (vector-ref acc degree)))
    (when (not (member v all-nodes))
      (let ((new-all (cons v all-nodes))
            (new-deg (cons v deg-nodes)))
        (vector-set! acc 0 new-all)
        (vector-set! acc degree new-deg)))
    acc))

(module+ test
  (check-equal? (add-node-by-degree (make-vector 6 '()) 'a 3)
                '#((a) () () (a) () ())))

; auxiliary function for get-graph-nodes-by-degrees
(define (add-edge-nodes-by-degrees* graph)
  (λ (edge acc)
    (let* ((v1 (car edge))
           (deg1 (get-degree v1 graph))
           (v2 (cadr edge))
           (deg2 (get-degree v2 graph)))
      (add-node-by-degree (add-node-by-degree acc v1 deg1) v2 deg2))))

(module+ test
  (check-equal? ((add-edge-nodes-by-degrees* '((a b))) '(a b) (make-vector 2 '()))
                '#((b a) (b a)))

  (let ((add-edge-nodes-by-degrees (add-edge-nodes-by-degrees* '((a b) (a c))))
        (acc (make-vector 3 '())))
    (check-equal? (add-edge-nodes-by-degrees '(a b) acc)
                  '#((b a) (b) (a)))
    (check-equal? acc '#((b a) (b) (a)))
    (check-equal? (add-edge-nodes-by-degrees '(a c) acc)
                  '#((c b a) (c b) (a)))
    ))

(define (get-graph-nodes-by-degrees graph nb-vertices)
  (vector-map reverse
              (foldl (add-edge-nodes-by-degrees* graph)
                     (make-vector nb-vertices '())
                     graph)))
(module+ test
  (check-equal? (get-graph-nodes-by-degrees '((a b) (a c)) 3)
                '#((a b c) (b c) (a)))
  (check-equal? (get-graph-nodes-by-degrees  '((0 3) (0 4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4)) 5)
                '#((0 3 4 1 2) () (0) (1 2) (3 4))))


(provide
 deep
 get-degree
 get-graph-degrees
 contains-all?
 graph->node-set graph-first-node graph-second-node get-graph-degree-1-vertices
 tailrec-is-graph-connected?
 get-graph-nodes-by-degrees
 )
#lang racket


(require srfi/67) ; compare procedures
(require "graph-utils.rkt")

(define (new-name-numeric-generator)
  (let ((count 0))
    (λ () (begin0
            count
            (set! count (+ 1 count))))))


         
; node-renamings: a hash map linking old vertices names to new ones
; new-names: a function returning a new unused name each time it is called
(define (rec-rename-graph-vertices graph node-renamings new-names)
  (if (null? graph)
      '()
      (let ((edge (car graph)))
        (cons (rename-edge-vertices edge node-renamings new-names)
              (rec-rename-graph-vertices (cdr graph) node-renamings new-names)))))

(define (rename-edge-vertices edge node-renamings new-names)
  (map (λ (vertex) (rename-vertex vertex node-renamings new-names))
       edge))

(define (rename-vertex vertex node-renamings new-names)
  (let ((renaming (hash-ref node-renamings vertex false)))
    (if renaming
        renaming
        (let ((new-name (new-names)))
          (hash-set! node-renamings vertex new-name)
          new-name))))

(define (get-degree-renaming graph nb-vertices new-names)
  (let ((vector-graph-nodes-by-degree (get-graph-nodes-by-degrees graph nb-vertices))
        (node-renamings (make-hash)))
    (vector-set! vector-graph-nodes-by-degree 0 '())
    (map (λ (_) (rename-vertex _ node-renamings new-names))
         (flatten (vector->list vector-graph-nodes-by-degree)))
    node-renamings))

(module+ test
  (require rackunit)
  (check-not-false (member (hash->list (get-degree-renaming '((0 1) (0 2)) 3 (new-name-numeric-generator)))
                           '(((0 . 2) (2 . 1) (1 . 0)) ((0 . 2) (1 . 0) (2 . 1))))))



(define (get-neighbours-min-degree vertex graph get-vertex-degree)
  (define vertex-degree (get-vertex-degree vertex))
  (define neighbours-greater-degree (filter (λ (v) (<= vertex-degree (get-vertex-degree v)))
    (get-neighbours vertex graph)))
  (min&args neighbours-greater-degree get-vertex-degree))

; in each edge, the vertices are sorted by their numbers.
; if n(v1) < n(v2) then deg(v1) <= deg(v2).
; if n(v1) < n(v2) and deg(v1) == deg(v2)
; then the degree of the least neighbour of v1 greater than v1 is less of equal to
; degree of the least neighbour of v2 greater than v2
; edges are sorted
(define (labelled-graph->unlabelled labelled)
  (define degrees (make-hash))
  (deep (λ (_) (hash-set! degrees _ (get-degree _ labelled))) labelled)
  (define (vertice-compare v1 v2)
    (refine-compare
        (integer-compare (hash-ref degrees v1) (hash-ref degrees v2))
        (integer-compare v1 v2)))
  (define (vertice<? v1 v2) (< (vertice-compare v1 v2)
                               0))
  ;TODO
  '()
)

(provide
 rec-rename-graph-vertices
 get-degree-renaming
 new-name-numeric-generator)
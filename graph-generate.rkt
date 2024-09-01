#lang racket

(require srfi/67) ; compare procedures
(require "combinatorics.rkt")
(require "graph-utils.rkt")
(require "graph-compare.rkt")
(require "graph-output.rkt")
(require "graph-random.rkt")


; generate all labelled graphs with vertices.
; no loop, no multiple edge.
; not necessarily containing all vertices
(define (generate-all-labelled-graphs vertices)
  (tailrec-parts '(()) (tailrec-couples '() vertices)))

(module+ test
  (require rackunit)
  (check-equal?
   (generate-all-labelled-graphs '(a e z))
   '(() ((a e)) ((a z)) ((a z) (a e)) ((e z)) ((e z) (a e)) ((e z) (a z)) ((e z) (a z) (a e)))))

(define (generate-all-labelled-graphs/n-edges nb-vertices nb-edges)
  (parts-w/nb (tailrec-couples '() (range nb-vertices)) nb-edges))

(module+ test
  (check-equal? (generate-all-labelled-graphs/n-edges 3 2) '(((0 2) (0 1)) ((1 2) (0 1)) ((1 2) (0 2)))))


(define (generate-labelled-graphs-all-vertices l)
  (filter (λ(g) (equal? #t (contains-all? g l))) (generate-all-labelled-graphs l)))

(module+ test
  (check-equal?
   (generate-labelled-graphs-all-vertices '(a e z))
   '(((a z) (a e)) ((e z) (a e)) ((e z) (a z)) ((e z) (a z) (a e)))))


; do not rename vertices
; reorder vertices in edges and edges in graph by ascending degree
; get-graph-degrees applied to the returned graph gives
; a lexicographically sorted suite of pairs of integer.
; Does not return a canonical form of an unlabelled graph.
(define (rewrite-graph graph)
  (define degrees (make-hash))
  (deep (λ (_) (hash-set! degrees _ (get-degree _ graph))) graph)
  (define (vertice-compare v1 v2)
    (refine-compare
        (integer-compare (hash-ref degrees v1) (hash-ref degrees v2))
        (integer-compare v1 v2)))
  (define (vertice<? v1 v2) (< (vertice-compare v1 v2)
                               0))
  (sort (map (λ (_) (sort _ vertice<?)) graph) (edge<?* (λ (_) (hash-ref degrees _)))))


(module+ test
  (check-equal?
   (get-graph-degrees (rewrite-graph '((0 1) (2 3) (1 3) (0 4) (1 4) (2 4))))
   '((2 2) (2 3) (2 3) (2 3) (2 3) (3 3)))

  (check-equal?
   (get-graph-degrees (rewrite-graph '((0 1) (2 3) (1 3) (0 4) (2 4) (3 4))))
   '((2 2) (2 3) (2 3) (2 3) (2 3) (3 3)))

  ;  2     0
  ;  |\   /|
  ;  | \ / |
  ;  3  4  |
  ;   \ | /
  ;    \|/
  ;     1
  (check-equal?
   (rewrite-graph '((0 1) (2 3) (1 3) (0 4) (1 4) (2 4)))
   '((2 3) (0 1) (0 4) (2 4) (3 1) (1 4)))

  ;  0     2
  ;  |\   /|
  ;  | \ / |
  ;  1  4  |
  ;   \ | /
  ;    \|/
  ;     3
  (check-equal?
   (rewrite-graph '((0 1) (2 3) (1 3) (0 4) (2 4) (3 4)))
   '((0 1) (0 4) (1 3) (2 3) (2 4) (3 4)))

  (check-equal?
   '()
   (get-graph-degree-1-vertices
    (rewrite-graph
     '((29 7) (17 9) (5 6) (4 15) (5 10) (19 29) (20 8) (17 5) (2 22) (24 18) (25 4) (17 13) (2 26)
              (9 10) (27 24) (23 10) (15 29) (12 6) (22 9) (15 18) (22 11) (2 21) (20 11) (29 12)
              (21 7) (2 8) (5 18) (15 20) (27 4) (7 2) (1 17) (16 8) (20 21) (8 22) (28 26) (18 17)
              (25 28) (2 18) (20 1) (5 20) (2 20) (3 23) (26 15) (24 19) (4 2) (18 15) (6 10) (21 22)
              (12 22) (0 21) (14 24) (0 26) (3 0) (13 14) (14 21) (16 29)))
    (range 30))))

(provide rewrite-graph)
#lang racket

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

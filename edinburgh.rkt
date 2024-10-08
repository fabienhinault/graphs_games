#lang racket

(require "graph-utils.rkt")
(require "graph-random.rkt")
(require "graph-generate.rkt")
(require "unlabelled-graph-generation.rkt")
(require "graph-output.rkt")

(define (create-random-edinburgh-graph vertices nb-edges)
  (define first (random-graph vertices nb-edges))
  (displayln first)
  (define all-vertices (add-absent-vertices first vertices))
  (displayln all-vertices)
  (define deduplicated (remove-duplicates all-vertices))
  (displayln deduplicated)
  (define connected (rec-connect-graph deduplicated (list->set vertices)))
  (displayln connected)
  (define deg2 (make-all-vertices-degree2 connected vertices))
  (displayln deg2)
  (define rewritten (rewrite-graph deg2))
  (displayln rewritten)
  (define renamed (rec-rename-graph-vertices rewritten (make-hash) (new-name-numeric-generator)))
  (displayln renamed)
  renamed)


(define (write-random-edinburgh-dot nb-vertices nb-edges)
  (let* ((vertices (range nb-vertices))
         (graph (create-random-edinburgh-graph vertices nb-edges))
         (final-nb-edges (length graph))
         (path (~a nb-vertices "/" final-nb-edges)))
    (write-dot-file graph path vertices)))

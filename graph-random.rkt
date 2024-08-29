#lang racket


(require racket/random)
(require "graph-utils.rkt")

(define (random-edge-from vertices vertex)
  (list vertex (random-ref (remove vertex vertices))))

(define (random-edge vertices)
  (random-edge-from vertices (random-ref vertices)))

(define (random-graph l nb-edges)
  (remove-duplicates (build-list nb-edges (λ (_) (random-edge l)))))


(define (add-absent-vertices g vertices)
  (let ((missing-vertices (contains-all? g vertices)))
    (if (equal? missing-vertices #t)
        g
        (let* ((result (append g
                               (map (λ (vertex) (random-edge-from vertices vertex))
                                    missing-vertices)))
               (missing-from-result (contains-all? result vertices)))
          (when (not (equal? missing-from-result #t)) (raise (list g '() '() result)))
          result))))


(define (make-all-vertices-degree2 graph vertices)
  (define result (append graph
                         (map (λ (vertex) (random-edge-from vertices vertex))
                              (get-graph-degree-1-vertices graph vertices))))
  (when (not (null? (get-graph-degree-1-vertices result vertices)))
    (raise (list graph '() '() result)))
  result)

(module+ test
  (require rackunit)
  (check-equal?
   '()
   (get-graph-degree-1-vertices
    (make-all-vertices-degree2
     '((3 26) (7 16) (20 8) (28 20) (7 22) (6 3) (28 1) (22 12) (27 11) (21 2) (15 6) (2 19) (26 14)
              (1 26) (0 5) (10 4) (16 12) (18 17) (1 11) (8 5) (5 9) (21 7) (12 21) (15 18) (22 1)
              (0 11) (27 8) (26 29) (5 15) (19 18) (10 16) (9 5) (9 1) (10 17) (18 21) (3 1) (16 7)
              (10 0) (29 4) (20 26) (1 29) (13 28) (19 2) (17 7) (8 17) (13 16) (24 3) (26 22) (23 20)
              (25 2))
     (range 30))
    (range 30))))


(define (rec-connect-graph g nodes-set)
  (let ((complete (tailrec-is-graph-connected? g nodes-set (set (graph-first-node g)) '())))
    (cond ((equal? complete #t)
           g)
          ((equal? complete #f)
           (raise g))
          (else 
           (rec-connect-graph (cons complete g) nodes-set)))))

(module+ test
  (define set-abcd  (set 'a 'b 'c 'd))
  (if (equal? (version) "8.9")
      (check-equal? (rec-connect-graph '((a b) (c d)) set-abcd) '((c b) (a b) (c d)))
      (check-equal? (rec-connect-graph '((a b) (c d)) set-abcd) '((c a) (a b) (c d)))))


(provide add-absent-vertices random-graph make-all-vertices-degree2
         rec-connect-graph)
#lang racket

(require "combinatorics.rkt")
(require "unlabelled-graph-common.rkt")

; first, next

(define (first-part categories nb filled-slot)
  (first-part/nb-max-categories categories nb))

;(define (first-nbs categories nb filled-slot)
;  (first-nbs/nb-max-categories (map category-vertices categories)
;                                 (get-maxes categories filled-slot) nb))

(define (next-part categories nb filled-slot)
  (next-part/nb-max-categories categories nb))

;(define (next-nbs categories nb filled-slot)
;  (next-nbs/nb-max-categories (map category-vertices categories)
;                                 (get-maxes categories filled-slot) nb part))

(define (first-graph/edges edges category-nbs degrees first-vertex new-all-categories
                           new-v1-categories first-second-same-category filled-slots)
  (define new-degrees (get-new-degrees edges (cdr degrees) first-vertex))
  (define new-new-v1-categories 
    (if first-second-same-category
        (get-nbs-categories category-nbs new-v1-categories (car filled-slots))
        new-all-categories))
  (define new-filled-slots
    (if (not first-second-same-category)
        (map (λ (v) 0) (category-vertices (car new-new-v1-categories)))
        (get-new-filled-slots edges first-vertex new-new-v1-categories (cdr filled-slots))))
  (define subgraph (first-graph  new-degrees (+ 1 first-vertex) new-all-categories
                          new-new-v1-categories))
  (if subgraph
      (append edges subgraph)
      (let* ((neighbours (next-part new-v1-categories (car degrees) (car filled-slots)))
             (category-nbs (next-nbs new-v1-categories (car degrees) (car filled-slots)))
             (next-edges (neighbours->edges first-vertex neighbours)))
        (if next-edges
            (first-graph/edges next-edges degrees first-vertex new-all-categories
                               new-v1-categories first-second-same-category)
            #f))))

(define (first-graph degrees first-vertex all-categories first-vertex-categories filled-slots)
  (define length-degrees (length degrees))
  (cond
    ((equal? degrees '()) #f)
    ((equal? degrees '(0)) '())
    ((and (equal? 0 (car degrees)) (memf (λ (deg) (> deg 0)) degrees))
     #f) ; break as leading to disconnected graphs
    ((null? (car all-categories))
     (first-graph degrees first-vertex (cdr all-categories) first-vertex-categories))
    ((null? (car first-vertex-categories))
     (first-graph degrees first-vertex all-categories (cdr first-vertex-categories)))
    ((memf (λ (deg) (>= deg length-degrees)) degrees)
     #f);break
    (else
     (let* ((first-second-same-category (member (+ 1 first-vertex) (car all-categories)))
            (new-all-categories (filter-out-vertex-from-categories first-vertex all-categories))
            (new-v1-categories (filter-out-vertex-from-categories first-vertex first-vertex-categories))
            (neighbours (first-part new-v1-categories (car degrees) (car filled-slots)))
            (category-nbs (first-nbs new-v1-categories (car degrees) (car filled-slots)))
            (edges (neighbours->edges first-vertex neighbours)))
       (if edges
           (first-graph/edges edges category-nbs degrees first-vertex new-all-categories
                              new-v1-categories first-second-same-category filled-slots)
           #f)))))


(module+ test
  (require rackunit)
  (check-equal? (first-graph '(0 2) 3 '((4)) '((4))) #f)
  (check-equal? (first-graph '(0) 1 '((1)) '((1))) '())
  (check-equal? (first-graph '(1 1) 0 '((0 1)) '((0 1))) '((0 1))) ; 0--1
  (check-equal? (first-graph '(0) 3 '((3)) '((3))) '())
  (check-equal? (first-graph '(1 1) 2 '((2 3)) '((2 3))) '((2 3)))
  (check-equal? (first-graph '(2) 3 '((3)) '((3))) #f)
  (check-equal? (first-graph '(1 3) 2 '((2) (3)) '((2) (3))) #f)
  (check-equal? (first-graph '(1 2 3) 1 '((1) (2) (3)) '((1) (2) (3))) #f)
  (check-equal? (first-graph '(2 2 3 3) 0 '((0 1) (2 3)) '((0 1) (2 3))) '((0 2) (0 3) (1 2) (1 3) (2 3)))
  (check-equal? (first-graph '(1) 4 '((4)) '((4))) #f)
  (check-equal? (first-graph '(0 2) 3 '((4)) '((4))) #f)
  (check-equal? (first-graph '(2 1 3) 2 '((2) (3) (4)) '((2) (3) (4))) #f)
  (check-equal? (first-graph '(1 2 3) 2 '((2) (3) (4)) '((2) (3) (4))) #f)
  (check-equal? (first-graph '(2 2 2) 2 '((2 3) (4)) '((4))) #f)
  (check-equal? (first-graph '(1 2 2 3) 1 '((1) (2) (3) (4)) '((1) (2) (3) (4))) '{(1 4) (2 3) (2 4) (3 4)})
  (check-equal? (first-graph '(1 2 2 3) 1 '((1) (2 3) (4)) '((1) (2 3) (4))) '{(1 4) (2 3) (2 4) (3 4)})
  (check-equal? (first-graph '(1 1 3 3) 1 '((1 2) (3 4)) '((1 2) (3 4))) #f)
  (check-equal? (first-graph '(2 2 2 3 3) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4)))
                '((0 1) (0 3) (1 4) (2 3) (2 4) (3 4)))
  (check-equal? (first-graph '(2 2 2 2 2) 0 '((0 1 2 3 4)) '((0 1 2 3 4)))
                '((0 1) (0 2) (1 3) (2 4) (3 4)))
  (check-equal? (first-graph '(2 2 2 2 4) 0 '((0 1 2 3) (4)) '((0 1 2 3) (4)))
                '((0 1) (0 4) (1 4) (2 3) (2 4) (3 4)))
  (check-equal? (first-graph '(2 2 2 4 4) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4)))
                '((0 3) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4)))
  (check-equal? (first-graph '(2 2 3 3 4) 0 '((0 1) (2 3) (4)) '((0 1) (2 3) (4)))
                '((0 2) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4)))
  (check-equal? (first-graph '(2 3 3 3 3) 0 '((0) (1 2 3 4)) '((0) (1 2 3 4)))
                '((0 1) (0 2) (1 3) (1 4) (2 3) (2 4) (3 4)))
  (check-equal? (first-graph '(2 2 4 4 4) 0 '((0 1) (2 3 4)) '((0 1) (2 3 4)))
                #f)
  (check-equal? (first-graph '(2 3 3 4 4) 0 '((0) (1 2) (3 4)) '((0) (1 2) (3 4)))
                '((0 3) (0 4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4)))
  (check-equal? (first-graph '(3 3 3 3 4) 0 '((0 1 2 3) (4)) '((0 1 2 3) (4)))
                '((0 1) (0 2) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4)))
  (check-equal? (first-graph '(2 2 2 2 2 2) 0 '((0 1 2 3 4 5)) '((0 1 2 3 4 5)))
                '((0 1) (0 2) (1 3) (2 4) (3 5) (4 5))))


(define (next-graph/edges edges category-nbs subgraph degrees first-vertex new-all-categories new-v1-categories
                          first-second-same-category filled-slots)
  (define new-degrees (get-new-degrees edges (cdr degrees) first-vertex))
  (define new-new-v1-categories 
    (if first-second-same-category
        (get-nbs-categories category-nbs new-v1-categories (car filled-slots))
        new-all-categories))
  (define new-filled-slots
    (if (not first-second-same-category)
        (map (λ (v) 0) (category-vertices (car new-new-v1-categories)))
        (get-new-filled-slots edges first-vertex new-new-v1-categories (cdr filled-slots))))
  (define next-subgraph (next-graph subgraph new-degrees (+ 1 first-vertex) new-new-all-categories
                          new-new-v1-categories))
  (if next-subgraph
      (append edges next-subgraph)
      (let* ((edge-categories (get-edge-categories first-vertex new-v1-categories))
             (next-edges (next-part/nb-categories edge-categories (car degrees) edges)))
        (if next-edges
            (first-graph/edges next-edges degrees first-vertex new-all-categories
                               new-v1-categories first-second-same-category)
            #f))))

(define (next-graph graph degrees first-vertex all-categories first-vertex-categories)
  (define length-degrees (length degrees))
  (cond
    ((equal? degrees '()) #f)
    ((equal? degrees '(0)) #f) ; '() was returned as first graph, no next
    ((and (equal? 0 (car degrees)) (memf (λ (deg) (> deg 0)) degrees))
     #f) ; break as leading to disconnected graphs
    ((null? (car all-categories))
     (next-graph graph degrees first-vertex (cdr all-categories) first-vertex-categories))
    ((null? (car first-vertex-categories))
     (next-graph graph degrees first-vertex all-categories (cdr first-vertex-categories)))
    ((memf (λ (deg) (>= deg length-degrees)) degrees)
     #f);break
    (else
     (let-values (((edges subgraph) (splitf-at graph (λ (edge) (equal? (car edge) first-vertex)))))
       (let* ((first-second-same-category (member (+ 1 first-vertex) (car all-categories)))
              (new-all-categories (filter-out-vertex-from-categories first-vertex all-categories))
              (new-v1-categories (filter-out-vertex-from-categories first-vertex first-vertex-categories)))
         (next-graph/edges edges subgraph degrees first-vertex new-all-categories new-v1-categories
                           first-second-same-category filled-slots))))))


(module+ test
  (check-equal? (next-graph '() '(0) 1 '((1)) '((1))) #f)
  (check-equal? (next-graph '((0 1)) '(1 1) 0 '((0 1)) '((0 1))) #f) ; 0--1
  (check-equal? (next-graph '() '(0) 3 '((3)) '((3))) #f)
  (check-equal? (next-graph '((2 3)) '(1 1) 2 '((2 3)) '((2 3))) #f)
  (check-equal? (next-graph '((0 2) (0 3) (1 2) (1 3) (2 3)) '(2 2 3 3) 0 '((0 1) (2 3)) '((0 1) (2 3))) #f)
  (check-equal? (next-graph '{(1 4) (2 3) (2 4) (3 4)} '(1 2 2 3) 1 '((1) (2) (3) (4)) '((1) (2) (3) (4))) #f)
  (check-equal? (next-graph '{(1 4) (2 3) (2 4) (3 4)} '(1 2 2 3) 1 '((1) (2 3) (4)) '((1) (2 3) (4)))  #f)
  (check-equal? (next-graph '((0 1) (0 3) (1 4) (2 3) (2 4) (3 4)) '(2 2 2 3 3) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4)))
                '((0 3) (0 4) (1 3) (1 4) (2 3) (2 4)))
  (check-equal? (next-graph '((0 1) (0 2) (1 3) (2 4) (3 4)) '(2 2 2 2 2) 0 '((0 1 2 3 4)) '((0 1 2 3 4)))
                #f)
  (check-equal? (next-graph '((0 1) (0 4) (1 4) (2 3) (2 4) (3 4)) '(2 2 2 2 4) 0 '((0 1 2 3) (4)) '((0 1 2 3) (4)))
                #f)
  (check-equal? (next-graph '((0 3) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4)) '(2 2 2 4 4) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4)))
                #f)
  (check-equal? (next-graph '((0 2) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4)) '(2 2 3 3 4) 0 '((0 1) (2 3) (4)) '((0 1) (2 3) (4)))
                #f)
  (check-equal? (next-graph '((0 1) (0 2) (1 3) (1 4) (2 3) (2 4) (3 4)) '(2 3 3 3 3) 0 '((0) (1 2 3 4)) '((0) (1 2 3 4)))
                #f)
  (check-equal? (next-graph '((0 3) (0 4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4)) '(2 3 3 4 4) 0 '((0) (1 2) (3 4)) '((0) (1 2) (3 4)))
                #f)
  (check-equal? (next-graph '((0 1) (0 2) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4)) '(3 3 3 3 4) 0 '((0 1 2 3) (4)) '((0 1 2 3) (4)))
                #f)
  (check-equal? (next-graph '((0 1) (0 2) (1 3) (2 4) (3 5) (4 5)) '(2 2 2 2 2 2) 0 '((0 1 2 3 4 5)) '((0 1 2 3 4 5)))
                #f))

;;;;;;;;;;;;;;;;

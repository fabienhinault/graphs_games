#lang racket


(require srfi/67) ; compare procedures
(require "graph-utils.rkt")
(require "combinatorics.rkt")
(require "graph-generate.rkt")

(module+ test
  (require rackunit))

(module+ test
  (define Xd40j '((0 1)
                  (0 26)
                  (1 27)
                  (2 11)
                  (2 27)
                  (3 12)
                  (3 26)
                  (4 13)
                  (4 16)
                  (5 17)
                  (5 28)
                  (6 17)
                  (6 28)
                  (7 23)
                  (7 29)
                  (8 23)
                  (8 24)
                  (9 25)
                  (9 26)
                  (10 24)
                  (10 27)
                  (11 14)
                  (11 29)
                  (12 14)
                  (12 18)
                  (13 14)
                  (13 16)
                  (15 19)
                  (15 20)
                  (15 26)
                  (16 28)
                  (16 29)
                  (17 24)
                  (17 29)
                  (18 23)
                  (19 18)
                  (19 23)
                  (19 28)
                  (20 18)
                  (20 28)
                  (20 29)
                  (21 22)
                  (21 25)
                  (21 26)
                  (21 29)
                  (22 23)
                  (22 24)
                  (22 25)
                  (24 25)
                  (25 27)
                  (26 27)
                  (27 28)))
  )


; first partition of (apply append categories) having nb elements,
; always taking the first elements in each category
(define (first-part/nb-categories categories nb)
    (cond ((null? categories)
           #f)
          ((equal? 0 nb)
           '())
          ((null? (car categories))
           (first-part/nb-categories (cdr categories) nb))
          (else
           (let ((sub (first-part/nb-categories
                       (cons (cdar categories) (cdr categories)) (- nb 1))))
             (if sub
                 (cons (caar categories) sub)
                 (first-part/nb-categories (cdr categories) nb))))))

(module+ test
  (check-equal? (first-part/nb-categories '((0)) 0) '())
  (check-equal? (first-part/nb-categories '(()) 0) '())
  (check-equal? (first-part/nb-categories '() 1) #f)
  (check-equal? (first-part/nb-categories '((0)) 1) '(0))
  (check-equal? (first-part/nb-categories '((0)) 2) #f)
  (check-equal? (first-part/nb-categories '((0 1)) 1) '(0))
  (check-equal? (first-part/nb-categories '(() (1)) 0) '())
  (check-equal? (first-part/nb-categories '((0) (1)) 1) '(0))
  (check-equal? (first-part/nb-categories '((0 1)) 2) '(0 1))
  (check-equal? (first-part/nb-categories '((0) (1)) 2) '(0 1))
  (check-equal? (first-part/nb-categories '((0 1 2)) 1) '(0))
  (check-equal? (first-part/nb-categories '((0) (1 2)) 1) '(0))
  (check-equal? (first-part/nb-categories '((0) (1 2)) 2) '(0 1))
  (check-equal? (first-part/nb-categories '((0 1) (2)) 1) '(0))
  (check-equal? (first-part/nb-categories '((0) (1) (2)) 1) '(0))
  (check-equal? (first-part/nb-categories '((0 1 2)) 2) '(0 1))
  (check-equal? (first-part/nb-categories '((0) (1) (2)) 2) '(0 1))
  (check-equal? (first-part/nb-categories '((0 1 2)) 3) '(0 1 2))
  (check-equal? (first-part/nb-categories '((0 1 2 3)) 1) '(0))
  (check-equal? (first-part/nb-categories '((0) (1 2 3)) 1) '(0))
  (check-equal? (first-part/nb-categories '((0 1) (2 3)) 1) '(0))
  (check-equal? (first-part/nb-categories '((0 1 2) (3)) 1) '(0))
  (check-equal? (first-part/nb-categories '((0 1 2 3)) 2) '(0 1))
  (check-equal? (first-part/nb-categories '((0) (1 2 3)) 2) '(0 1))
  (check-equal? (first-part/nb-categories '((0 1) (2 3)) 2) '(0 1))
  (check-equal? (first-part/nb-categories '((0 1 2) (3)) 2) '(0 1))
  (check-equal? (first-part/nb-categories '((0 1 2 3)) 3) '(0 1 2))
  (check-equal? (first-part/nb-categories '((0) (1 2 3)) 3) '(0 1 2))
  (check-equal? (first-part/nb-categories '((0 1) (2 3)) 3) '(0 1 2))
  (check-equal? (first-part/nb-categories '((0 1 2 3)) 4) '(0 1 2 3))
  (check-equal? (first-part/nb-categories '((0 1) (2 3)) 4) '(0 1 2 3))
  (check-equal? (first-part/nb-categories '(([0 1]) ([0 2] [0 3])) 2) '([0 1] [0 2]))
  (check-equal? (first-part/nb-categories '(([1 2]) ([1 3])) 1) '([1 2])))

; next partition of (apply append categories) having nb elements,
; always taking the first elements in each category
(define (next-part/nb-categories categories nb part)
    (cond ((null? categories)
           #f)
          ((equal? 0 nb)
           #f)
          ((null? (car categories))
           (next-part/nb-categories (cdr categories) nb part))
          ((equal? (car part) (caar categories))
               (let ((sub (next-part/nb-categories
                           (cons (cdar categories) (cdr categories)) (- nb 1) (cdr part))))
                 (if sub
                     (cons (car part) sub)
                     (first-part/nb-categories (cdr categories) nb))))
          (else
           (next-part/nb-categories (cdr categories) nb part))))

(module+ test
  (check-equal? (next-part/nb-categories '((0)) 0 '()) #f)
  (check-equal? (next-part/nb-categories '(()) 0 '()) #f)
  (check-equal? (next-part/nb-categories '() 1 '()) #f)
  (check-equal? (next-part/nb-categories '((0)) 1 '(0)) #f)
  (check-equal? (next-part/nb-categories '((0 1)) 1 '(0)) #f)
  (check-equal? (next-part/nb-categories '(() (1)) 0 '()) #f)
  (check-equal? (next-part/nb-categories '((0) (1)) 1 '(0)) '(1))
  (check-equal? (next-part/nb-categories '((0) (1)) 1 '(1)) #f)
  (check-equal? (next-part/nb-categories '((0 1)) 2 '(0 1)) #f)
  (check-equal? (next-part/nb-categories '((0) (1)) 2 '(0 1)) #f)
  (check-equal? (next-part/nb-categories '((0 1 2)) 1 '(0)) #f)
  (check-equal? (next-part/nb-categories '((0) (1 2)) 1 '(0)) '(1))
  (check-equal? (next-part/nb-categories '((0) (1 2)) 1 '(1)) #f)
  (check-equal? (next-part/nb-categories '((0) (1 2)) 2 '(0 1)) '(1 2))
  (check-equal? (next-part/nb-categories '((0) (1 2)) 2 '(1 2)) #f)
  (check-equal? (next-part/nb-categories '((0 1) (2)) 1 '(0)) '(2))
  (check-equal? (next-part/nb-categories '((0 1) (2)) 1 '(2)) #f)
  (check-equal? (next-part/nb-categories '((0) (1) (2)) 1 '(0)) '(1))
  (check-equal? (next-part/nb-categories '((0) (1) (2)) 1 '(1)) '(2))
  (check-equal? (next-part/nb-categories '((0) (1) (2)) 1 '(2)) #f)
  (check-equal? (next-part/nb-categories '((0 1 2)) 2 '(0 1)) #f)
  (check-equal? (next-part/nb-categories '((0) (1) (2)) 2 '(0 1)) '(0 2))
  (check-equal? (next-part/nb-categories '((0) (1) (2)) 2 '(0 2)) '(1 2))
  (check-equal? (next-part/nb-categories '((0) (1) (2)) 2 '(1 2)) #f)
  (check-equal? (next-part/nb-categories '((0 1 2)) 3 '(0 1 2)) #f)
  (check-equal? (next-part/nb-categories '((0 1 2 3)) 1 '(0)) #f)
  (check-equal? (next-part/nb-categories '((0) (1 2 3)) 1 '(0)) '(1))
  (check-equal? (next-part/nb-categories '((0) (1 2 3)) 1 '(1)) #f)
  (check-equal? (next-part/nb-categories '((0 1) (2 3)) 1 '(0)) '(2))
  (check-equal? (next-part/nb-categories '((0 1 2) (3)) 1 '(0)) '(3))
  (check-equal? (next-part/nb-categories '((0 1 2 3)) 2 '(0 1)) #f)
  (check-equal? (next-part/nb-categories '((0) (1 2 3)) 2 '(0 1)) '(1 2))
  (check-equal? (next-part/nb-categories '((0) (1 2 3)) 2 '(1 2)) #f)
  (check-equal? (next-part/nb-categories '((0 1) (2 3)) 2 '(0 1)) '(0 2))
  (check-equal? (next-part/nb-categories '((0 1) (2 3)) 2 '(0 2)) '(2 3))
  (check-equal? (next-part/nb-categories '((0 1) (2 3)) 2 '(2 3)) #f)
  (check-equal? (next-part/nb-categories '((0 1 2) (3)) 2 '(0 1)) '(0 3))
  (check-equal? (next-part/nb-categories '((0 1 2) (3)) 2 '(0 3)) #f)
  (check-equal? (next-part/nb-categories '((0 1 2 3)) 3 '(0 1 2)) #f)
  (check-equal? (next-part/nb-categories '((0) (1 2 3)) 3 '(0 1 2)) '(1 2 3))
  (check-equal? (next-part/nb-categories '((0) (1 2 3)) 3 '(1 2 3)) #f)
  (check-equal? (next-part/nb-categories '((0 1) (2 3)) 3 '(0 1 2)) '(0 2 3))
  (check-equal? (next-part/nb-categories '((0 1) (2 3)) 3 '(0 2 3)) #f)
  (check-equal? (next-part/nb-categories '((0 1 2 3)) 4 '(0 1 2 3)) #f)
  (check-equal? (next-part/nb-categories '((0 1) (2 3)) 4 '(0 1 2 3)) #f)
  (check-equal? (next-part/nb-categories '(([0 1]) ([0 2] [0 3])) 2 '([0 1] [0 2])) '([0 2] [0 3]))
  (check-equal? (next-part/nb-categories '(([0 1]) ([0 2] [0 3])) 2 '([0 2] [0 3])) #f)
  (check-equal? (next-part/nb-categories '(([1 2]) ([1 3])) 1 '([1 2])) '([1 3]))
  (check-equal? (next-part/nb-categories '(([1 2]) ([1 3])) 1 '([1 3])) #f))

 
(define (get-edge-categories vertex vertex-categories)
  (map (λ (v-categorie)
         (map (λ (v) (list vertex v))
              v-categorie))
       vertex-categories))

(module+ test
  (check-equal? (get-edge-categories 0 '((1) (2))) '(((0 1)) ((0 2))))
  (check-equal? (get-edge-categories 0 '((1 2) (3))) '(((0 1) (0 2)) ((0 3))))
  (check-equal? (get-edge-categories 0 '((1) (2 3))) '(((0 1)) ((0 2) (0 3))))
  (check-equal? (get-edge-categories 1 '((2) (3))) '(((1 2)) ((1 3)))))


; removes 1 to the degrees of vertices strictly after first-vertex appearing in edges
; edges starting with first-vertex which will decrement degrees
; degrees of vertices strictly after first-vertex
; params
; edges: beginning with first-vertex which we consider to decrease degrees
; degrees: vertices degrees, first-vertex' degree beeing removed
; first-vertex
(define (get-new-degrees edges degrees first-vertex)
  (define second-vertices (map cadr edges))
  (define offset-second-vertices (map (λ (v) (- v first-vertex 1)) second-vertices))
  (foldl (λ (i degs) (list-update degs i (λ (d) (- d 1))))
         degrees
         offset-second-vertices))

(module+ test
  (check-equal? (get-new-degrees '((0 1) (0 2)) '(1 1) 0) '(0 0))
  (check-equal? (get-new-degrees '((0 1) (0 2)) '(2 3 3) 0) '(1 2 3)))



; return the categories after the one of the first edge in edges
; in the recursive process of degrees->graphs
; we must not edge the first vertex to vertices of a category
; if the first vertex in the previous recursion step edged only ot vertices of higher categories.
(define (remove-categories-before-first-joined categories edges)
  (if (null? edges)
      categories
      (memf (λ (categorie) (member (cadr (car edges)) categorie))
            categories)))

(module+ test
  (check-equal? (remove-categories-before-first-joined '((4)) '()) '((4)))

  ; as 0 edged only to the category '(3 4), category '(1 2) is forbidden to following vertices.
  (check-equal? (remove-categories-before-first-joined '((1 2) (3 4)) '((0 3) (0 4))) '((3 4))))


; split categories if some vertices are touched by edges
(define (get-new-new-categories new-categories edges)
  (let ((vertices  (map cadr edges)))
    (filter not-null?
            (append* (map (λ (categorie)
                            (call-with-values
                             (λ () (partition (λ (v) (member v vertices)) categorie))
                             list))
                          new-categories)))))

(module+ test
  (check-equal? (get-new-new-categories '((1 2 3)) '((0 1) (0 2))) '((1 2) (3)))
  (check-equal? (get-new-new-categories '((1) (2 3)) '((0 1) (0 2))) '((1) (2) (3))))



(define (filter-out-vertex-from-categories vertex categories)
  (filter-not
   null?
   (map (λ (categorie) (filter-not (λ (v) (equal? v vertex)) categorie))
        categories)))

(module+ test
  (check-equal? (filter-out-vertex-from-categories 0 '((0 1) (2 3))) '((1) (2 3)))
  (check-equal? (filter-out-vertex-from-categories 1 '((1) (2) (3))) '((2) (3))))
                                                                                                         

; in: degrees                    list of vertices' degrees.
; in: first-vertex               integer. Value of the first-vertex of the returned graphs.
; in: all-categories             gathering of vertices. First, a category correspond to vertices of same degree.
; during the recursion, categories can be splitted. The first vertex will only edge to the first
; vertices of any category.
; in: first-vertex-categories    subset of all-categories to which first-vertex is allowed to edge,
; depending on how the previous vertex has edged.
; in: used-vertices              set of vertices which are already used in previous edges,
; in previous recursions (not present here)
; return: list of graphs matching these degrees

;(define (rec-degrees->graphs degrees first-vertex all-categories first-vertex-categories used-vertices)


(define (first-graph/edges edges degrees first-vertex new-all-categories
                           new-v1-categories first-second-same-category)
  (define new-degrees (get-new-degrees edges (cdr degrees) first-vertex))
  (define new-new-all-categories (get-new-new-categories new-all-categories edges))
  (define new-new-v1-categories 
    (if first-second-same-category
        (get-new-new-categories
         (remove-categories-before-first-joined new-v1-categories edges)
         edges)
        new-new-all-categories))
  (define subgraph (first-graph  new-degrees (+ 1 first-vertex) new-new-all-categories
                          new-new-v1-categories))
  (if subgraph
      (append edges subgraph)
      (let* ((edge-categories (get-edge-categories first-vertex new-v1-categories))
             (next-edges (next-part/nb-categories edge-categories (car degrees) edges)))
        (if next-edges
            (first-graph/edges next-edges degrees first-vertex new-all-categories
                               new-v1-categories first-second-same-category)
            #f))))

(define (first-graph degrees first-vertex all-categories first-vertex-categories)
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
            (edge-categories (get-edge-categories first-vertex new-v1-categories))
            (edges (first-part/nb-categories edge-categories (car degrees))))
       (if edges
           (first-graph/edges edges degrees first-vertex new-all-categories
                              new-v1-categories first-second-same-category)
           #f)))))


(module+ test
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


(define (next-graph/edges edges subgraph degrees first-vertex new-all-categories new-v1-categories
                          first-second-same-category)
  (define new-degrees (get-new-degrees edges (cdr degrees) first-vertex))
  (define new-new-all-categories (get-new-new-categories new-all-categories edges))
  (define new-new-v1-categories 
    (if first-second-same-category
        (get-new-new-categories
         (remove-categories-before-first-joined new-v1-categories edges)
         edges)
        new-new-all-categories))
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
                           first-second-same-category))))))


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
                #f)

  (check-equal? (next-graph Xd40j
                '(2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 4 4 5 5 5 6 6 6 6) 0
               '((0 1 2 3 4 5 6 7 8 9 10) (11 12 13 14 15) (16 17 18 19 20 21 22) (23 24 25) (26 27 28 29))
               '((0 1 2 3 4 5 6 7 8 9 10) (11 12 13 14 15) (16 17 18 19 20 21 22) (23 24 25) (26 27 28 29)))
                '((0 1)
    (0 26)
    (1 27)
    (2 11)
    (2 27)
    (3 12)
    (3 26)
    (4 13)
    (4 16)
    (5 17)
    (5 28)
    (6 17)
    (6 28)
    (7 23)
    (7 29)
    (8 23)
    (8 24)
    (9 25)
    (9 26)
    (10 24)
    (10 27)
    (11 14)
    (11 29)
    (12 14)
    (12 18)
    (13 15)
    (13 16)
    (14 15)
    (15 16)
    (16 17)
    (17 18)
    (18 19)
    (18 20)
    (19 20)
    (19 21)
    (19 22)
    (20 21)
    (20 22)
    (21 22)
    (21 23)
    (22 23)
    (23 24)
    (24 25)
    (24 26)
    (25 27)
    (25 28)
    (25 29)
    (26 28)
    (26 29)
    (27 28)
    (27 29)
    (28 29)))

  )
   
(define (get-degrees-categories degrees first-vertex)
  (let* ((first-degree (car degrees))
         (index (index-where degrees (λ (d) (not (equal? d first-degree))))))
    (if index
        (cons (range first-vertex (+ first-vertex index))
              (get-degrees-categories (drop degrees index) (+ first-vertex index)))
        (list (range first-vertex (+ first-vertex (length degrees)))))))

(module+ test
  (check-equal? (get-degrees-categories '(2 2 3 4 4 5) 0)
                '((0 1) (2) (3 4) (5))))

(define (graph->degrees g max-vertex)
  (define degrees (make-vector (+ 1 max-vertex)))
  (for-each
   (λ (edge)
     (for-each
      (λ (vertex)
        (vector-set! degrees vertex (+ 1 (vector-ref degrees vertex))))
      edge))
   g)
  (vector->list degrees))

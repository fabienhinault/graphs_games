#lang racket

(require rackunit)
(require srfi/67) ; compare procedures
(require racket/random)
(require racket/trace)
(require json)
(require "combinatorics.rkt")
(require "graph-compare.rkt")
(require "graph-output.rkt")
(require "graph-random.rkt")
(require "graph-utils.rkt")
(require "graph-generate.rkt")
(require "unlabelled-graph-generation.rkt")
(require "edinburgh.rkt")
     






(define (get-new-edgess n)
  (let* ((new-node (- n 1))
         (old-nodes (range (- n 1)))
         (new-edges (map (λ (_) (list _ new-node)) old-nodes)))
    ; remove '() at start
    (cdr (tailrec-parts '(()) new-edges))))

(check-equal? (get-new-edgess 0) '())
(check-equal? (get-new-edgess 1) '())
(check-equal? (get-new-edgess 2) '(((0 1))))
(check-equal? (get-new-edgess 3) '(((0 2)) ((1 2)) ((1 2) (0 2))))
(check-equal? (get-new-edgess 4)
              '(((0 3)) ((1 3))
                            ((1 3) (0 3)) 
                            ((2 3))
                            ((2 3) (0 3)) ((2 3) (1 3))
                            ((2 3) (1 3) (0 3))))


; old-graph: a graph of (n - 1) vertices
; new-edgess: a list of lists of edges, all possibilities to link
; vertex n with some of the (n - 1) others.
(define (new-graphs old-graph new-edgess new-nb-vertices)
  ; need to rewrite twice to reorder vertices in edges after rename
  (map rewrite-graph
       (map
        (λ (_)
          (rec-rename-graph-vertices
           _
           (get-degree-renaming _ new-nb-vertices (new-name-numeric-generator))
           (λ () (raise 'error))))
        (map rewrite-graph
             (map (λ (_) (append old-graph _)) new-edgess)))))

(check-equal? (new-graphs '() (get-new-edgess 2) 2) '(((0 1))))


(define (new-graph old-graph new-edges)
  ; need to rewrite twice to reorder vertices in edges after rename
  (rewrite-graph
   (rec-rename-graph-vertices (rewrite-graph (append old-graph new-edges))
                              (make-hash) (new-name-numeric-generator))))


;                          (1 0) (3 2) (0 2)    (1 4) (2 4) (3 4)
(check-equal? (new-graph '((0 1) (2 3) (1 3)) '((0 4) (2 4) (3 4)))
              '((0 1) (0 2) (1 3) (4 2) (4 3) (2 3)))

(check-equal? (new-graph '((0 1) (2 3) (1 3)) '((0 4) (1 4) (2 4)))
              '((0 1) (0 4) (1 3) (2 3) (2 4) (3 4)))


(define (graphs4 nb-vertices graphs-n-1)
  (let ((new-edges (get-new-edgess nb-vertices)))
    (remove-duplicates (apply append (map (λ (_) (new-graphs _ new-edges nb-vertices)) graphs-n-1)))))

(check-equal? (graphs4 2 '(())) '(((0 1))))
(check-equal? (graphs4 3 '(((0 1))))
              '(((0 2) (1 2)) ((0 1) (0 2) (1 2))))
(check-equal? (graphs4 4 '(((0 1) (2 1)) ((0 1) (0 2) (1 2))))
              '(((0 2) (1 3) (2 3))
                ((0 3) (1 3) (2 3))
                ((0 3) (1 2) (1 3) (2 3))
                ((0 1) (0 2) (1 3) (2 3))
                ((0 2) (0 3) (1 2) (1 3) (2 3))
                ((0 1) (0 2) (0 3) (1 2) (1 3) (2 3))))

(define (has-vertex-degree-1 graph)
  (member 1 (flatten (deep (λ (_) (get-degree _ graph)) graph))))

(define (has-no-vertex-degree-1 graph)
  (not (has-vertex-degree-1 graph)))

(check-equal? (has-vertex-degree-1 '((0 1) (2 3) (1 3))) '(1 2 1 2 2 2))
(check-false
 (has-vertex-degree-1 '((0 1) (0 2) (0 3) (1 2) (1 3) (2 3))))
 
(define graphs-by-node-nb (make-vector 100))
(vector-set! graphs-by-node-nb 1 '(()))
(for ((i-node-nb (range 2 7)))
  (vector-set! graphs-by-node-nb i-node-nb
                 (graphs4 i-node-nb (vector-ref graphs-by-node-nb (- i-node-nb 1)))))

(define (pick-random-vertex graph forbidden-vertex)
  (let* ((picked-edge (random-ref graph))
         (picked-vertex (car picked-edge)))
    (if (not (equal? picked-vertex forbidden-vertex))
        picked-vertex
        (let ((picked-vertex (cadr picked-edge)))
          (if (not (equal? picked-vertex forbidden-vertex))
              picked-vertex
              (pick-random-vertex graph forbidden-vertex))))))

(define (add-random-edge graph vertex)
  (cons
   (list vertex
         (pick-random-vertex graph vertex))
   graph))




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

;(for-each
;   (lambda (_) (write-dot-file _ 6))
;   (filter has-no-vertex-degree-1 _6))

(define (degreess nb-vertices total-degree min-degree max-degree)
  (if (equal? nb-vertices 1)
      (if (or (> min-degree total-degree) (< max-degree total-degree))
          '()
          `((,total-degree)))
      (foldl 
       (λ (degree acc)
         (append acc
                 (map (λ (degrees) (cons degree degrees))
                      (degreess (- nb-vertices 1) (- total-degree degree) degree max-degree))))
       '()
       (range min-degree (+ 1 (quotient (- total-degree min-degree) (- nb-vertices 1)))))))

(check-equal? (degreess 1 2 2 2) '((2)))
(check-equal? (degreess 2 4 2 2) '((2 2)))
(check-equal? (degreess 3 6 2 2) '((2 2 2)))
(check-equal? (degreess 4 8 2 3) '((2 2 2 2)))
(check-equal? (degreess 4 10 2 3) '((2 2 3 3)))
(check-equal? (degreess 4 12 2 3) '((3 3 3 3)))
(check-equal? (degreess 5 10 2 4) '((2 2 2 2 2)))
(check-equal? (degreess 5 12 2 4) '((2 2 2 2 4) (2 2 2 3 3)))
(check-equal? (degreess 5 14 2 4) '((2 2 2 4 4) (2 2 3 3 4) (2 3 3 3 3)))
(check-equal? (degreess 5 16 2 4) '((2 2 4 4 4) (2 3 3 4 4) (3 3 3 3 4)))
(check-equal? (degreess 5 18 2 4) '((2 4 4 4 4) (3 3 4 4 4)))
(check-equal? (degreess 5 20 2 4) '((4 4 4 4 4)))
(check-equal? (degreess 6 12 2 5) '((2 2 2 2 2 2)))
(check-equal? (degreess 6 14 2 5) '((2 2 2 2 2 4) (2 2 2 2 3 3)))
(check-equal? (degreess 6 16 2 5) '((2 2 2 2 3 5) (2 2 2 2 4 4) (2 2 2 3 3 4) (2 2 3 3 3 3)))
(check-equal? (degreess 6 18 2 5) '((2 2 2 2 5 5)
                                    (2 2 2 3 4 5)
                                    (2 2 2 4 4 4)
                                    (2 2 3 3 3 5)
                                    (2 2 3 3 4 4)
                                    (2 3 3 3 3 4)
                                    (3 3 3 3 3 3)))
(check-equal? (degreess 6 20 2 5) '((2 2 2 4 5 5)
                                    (2 2 3 3 5 5)
                                    (2 2 3 4 4 5)
                                    (2 2 4 4 4 4)
                                    (2 3 3 3 4 5)
                                    (2 3 3 4 4 4)
                                    (3 3 3 3 3 5)
                                    (3 3 3 3 4 4)))
(check-equal? (degreess 6 22 2 5) '((2 2 3 5 5 5)
                                    (2 2 4 4 5 5)
                                    (2 3 3 4 5 5)
                                    (2 3 4 4 4 5)
                                    (2 4 4 4 4 4)
                                    (3 3 3 3 5 5)
                                    (3 3 3 4 4 5)
                                    (3 3 4 4 4 4)))
(check-equal? (degreess 6 24 2 5)   '((2 2 5 5 5 5)
                                      (2 3 4 5 5 5)
                                      (2 4 4 4 5 5)
                                      (3 3 3 5 5 5)
                                      (3 3 4 4 5 5)
                                      (3 4 4 4 4 5)
                                      (4 4 4 4 4 4)))
(check-equal? (degreess 6 26 2 5) '((2 4 5 5 5 5) (3 3 5 5 5 5) (3 4 4 5 5 5) (4 4 4 4 5 5)))
(check-equal? (degreess 6 28 2 5) '((3 5 5 5 5 5) (4 4 5 5 5 5)))
(check-equal? (degreess 6 30 2 5) '((5 5 5 5 5 5)))


(define (get-edge-categories vertex vertex-categories)
  (map (λ (categorie)
         (map (λ (v) (list vertex v))
              categorie))
       vertex-categories))

(check-equal? (get-edge-categories 0 '((1) (2))) '(((0 1)) ((0 2))))
(check-equal? (get-edge-categories 0 '((1 2) (3))) '(((0 1) (0 2)) ((0 3))))
(check-equal? (get-edge-categories 0 '((1) (2 3))) '(((0 1)) ((0 2) (0 3))))
(check-equal? (get-edge-categories 1 '((2) (3))) '(((1 2)) ((1 3))))

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

(check-equal? (get-new-degrees '((0 1) (0 2)) '(1 1) 0) '(0 0))
(check-equal? (get-new-degrees '((0 1) (0 2)) '(2 3 3) 0) '(1 2 3))

(define (not-null? l)
  (not (null? l)))

; return the categories after the one of the first edge in edges
; in the recursive process of degrees->graphs
; we must not edge the first vertex to vertices of a category
; if the first vertex in the previous recursion step edged only ot vertices of higher categories.
(define (remove-categories-before-first-joined categories edges)
  (if (null? edges)
      categories
      (memf (λ (categorie) (member (cadr (car edges)) categorie))
            categories)))

(check-equal? (remove-categories-before-first-joined '((4)) '()) '((4)))

; as 0 edged only to the category '(3 4), category '(1 2) is forbidden to following vertices.
(check-equal? (remove-categories-before-first-joined '((1 2) (3 4)) '((0 3) (0 4))) '((3 4)))

; split categories if some vertices are touched by edges
(define (get-new-new-categories new-categories edges)
  (let ((vertices  (map cadr edges)))
    (filter not-null?
            (append* (map (λ (categorie)
                            (call-with-values
                             (λ () (partition (λ (v) (member v vertices)) categorie))
                             list))
                          new-categories)))))

(check-equal? (get-new-new-categories '((1 2 3)) '((0 1) (0 2))) '((1 2) (3)))
(check-equal? (get-new-new-categories '((1) (2 3)) '((0 1) (0 2))) '((1) (2) (3)))

(define (filter-out-vertex-from-categories vertex categories)
  (filter-not
   null?
   (map (λ (categorie) (filter-not (λ (v) (equal? v vertex)) categorie))
        categories)))

(check-equal? (filter-out-vertex-from-categories 0 '((0 1) (2 3))) '((1) (2 3)))
(check-equal? (filter-out-vertex-from-categories 1 '((1) (2) (3))) '((2) (3)))

(define (get-edges-subgraphs edges degrees first-vertex new-all-categories
                             new-v1-categories first-second-same-category)
  (define new-degrees (get-new-degrees edges (cdr degrees) first-vertex))
  (define new-new-all-categories (get-new-new-categories new-all-categories edges))
  (define new-new-v1-categories 
    (if first-second-same-category
        (get-new-new-categories
         (remove-categories-before-first-joined new-v1-categories edges)
         edges)
        new-new-all-categories))
  (define sub-graphs (rec-degrees->graphs new-degrees
                                      (+ 1 first-vertex)
                                      new-new-all-categories
                                      new-new-v1-categories))
  (if (null? sub-graphs)
      '()
      (map (λ (sub-graph)
             (append edges sub-graph))
           sub-graphs)))

; in: degrees list of vertices' degrees.
; in: all-categories gathering of vertices. First, a category correspond to vertices of same degree.
; during the recursion, categories can be splitted. The first vertex will only edge to the first
; vertices of any category.
; in: first-vertex-categories subset of all-categories to which first-vertex is allowed to edge,
; depending on how the previous vertex has edged.
; return: list of graphs matching these degrees
(define (rec-degrees->graphs degrees first-vertex all-categories first-vertex-categories)
  (define length-degrees (length degrees))
  (cond ((equal? degrees '())
         '());break
        ((equal? degrees '(0))
         '(())) ; the empty graph
        ((and (equal? 0 (car degrees)) (memf (λ (deg) (> deg 0)) degrees))
         '()) ; break as leading to disconnected graphs
        ((null? (car all-categories))
         (rec-degrees->graphs degrees first-vertex (cdr all-categories) first-vertex-categories))
        ((null? (car first-vertex-categories))
         (rec-degrees->graphs degrees first-vertex all-categories (cdr first-vertex-categories)))
        ((memf (λ (deg) (>= deg length-degrees)) degrees)
         '());break
        (else (let* ((first-second-same-category (member (+ 1 first-vertex) (car all-categories)))
                     ;remove current vertex from categories
                     (new-all-categories (filter-out-vertex-from-categories first-vertex all-categories))
                     (new-v1-categories (filter-out-vertex-from-categories first-vertex first-vertex-categories))
                     (edge-categories (get-edge-categories first-vertex new-v1-categories))
                     (edgess (rec-parts-w/nb-categories edge-categories (car degrees))))
                 (append-map
                  (λ (edges)
                    (get-edges-subgraphs edges degrees first-vertex new-all-categories
                                         new-v1-categories first-second-same-category))
                  edgess)))))


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
              '((0 1) (0 2) (1 3) (2 4) (3 5) (4 5)))


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

;;;;;;;;;;;;;;;;
; stream version

(define (get-edges-subgraphs-stream edges degrees first-vertex new-all-categories
                                    new-v1-categories first-second-same-category)
  (define new-degrees (get-new-degrees edges (cdr degrees) first-vertex))
  (define new-new-all-categories (get-new-new-categories new-all-categories edges))
  (define new-new-v1-categories 
    (if first-second-same-category
        (get-new-new-categories
         (remove-categories-before-first-joined new-v1-categories edges)
         edges)
        new-new-all-categories))
  (define sub-graphs-stream (rec-degrees->graphs-stream new-degrees
                                                    (+ 1 first-vertex)
                                                    new-new-all-categories
                                                    new-new-v1-categories))
  (if (stream-empty? sub-graphs-stream)
      empty-stream
      (stream-map (λ (sub-graph) (append edges sub-graph))
                  sub-graphs-stream)))

; in: degrees list of vertices' degrees.
; in: all-categories gathering of vertices. First, a category correspond to vertices of same degree.
; during the recursion, categories can be splitted. The first vertex will only edge to the first
; vertices of any category.
; in: first-vertex-categories subset of all-categories to which first-vertex is allowed to edge,
; depending on how the previous vertex has edged.
; return: list of graphs matching these degrees
(define (rec-degrees->graphs-stream degrees first-vertex all-categories first-vertex-categories)
  (define length-degrees (length degrees))
  (cond ((equal? degrees '())
         empty-stream)
        ((equal? degrees '(0))
          (stream '())) ; the empty graph
        ((and (equal? 0 (car degrees)) (memf (λ (deg) (> deg 0)) degrees))
         empty-stream) ; break as leading to disconnected graphs
        ((null? (car all-categories))
         (rec-degrees->graphs-stream degrees first-vertex (cdr all-categories) first-vertex-categories))
        ((null? (car first-vertex-categories))
         (rec-degrees->graphs-stream degrees first-vertex all-categories (cdr first-vertex-categories)))

        ((memf (λ (deg) (>= deg length-degrees)) degrees)
         empty-stream)
        (else (let* ((first-second-same-category (member (+ 1 first-vertex) (car all-categories)))
                     ;remove current vertex from categories
                     (new-all-categories (filter-out-vertex-from-categories first-vertex all-categories))
                     (new-v1-categories (filter-out-vertex-from-categories first-vertex first-vertex-categories))
                     (edge-categories (get-edge-categories first-vertex new-v1-categories))
                     (edgess (rec-parts-w/nb-categories-stream edge-categories (car degrees))))
                (stream-fold
                  (λ (acc edges)
                    (stream-append
                     acc
                     (get-edges-subgraphs-stream edges degrees first-vertex new-all-categories
                                                new-v1-categories first-second-same-category)))
                  empty-stream
                  edgess)))))

(struct graph-gen-data (edges degrees first-vertex all-categories first-vertex-categories))

(define (get-edges-gen-data-stream data-edges edges degrees first-vertex new-all-categories
                                    new-v1-categories first-second-same-category)
  (define new-degrees (get-new-degrees edges (cdr degrees) first-vertex))
  (define new-new-all-categories (get-new-new-categories new-all-categories edges))
  (define new-new-v1-categories 
    (if first-second-same-category
        (get-new-new-categories
         (remove-categories-before-first-joined new-v1-categories edges)
         edges)
        new-new-all-categories))
  (graph-gen-data (append data-edges edges) new-degrees (+ 1 first-vertex) new-new-all-categories
                  new-new-v1-categories))

(define (tail-degrees->graphs-stream gen-datas)
  (if (stream-empty? gen-datas) empty-stream
      (let* ((data (stream-first gen-datas))
             (data-edges (graph-gen-data-edges data))
             (degrees (graph-gen-data-degrees data))
             (first-vertex (graph-gen-data-first-vertex data))
             (all-categories (graph-gen-data-all-categories data))
             (first-vertex-categories (graph-gen-data-first-vertex-categories data)))
        (define length-degrees (length degrees))
        (cond
          ((or (equal? degrees '()) (equal? degrees '(0)))
           (stream-cons data-edges (tail-degrees->graphs-stream (stream-rest gen-datas))))
          ((and (equal? 0 (car degrees)) (memf (λ (deg) (> deg 0)) degrees))
           ;do not go further on these edges, the graph will be disconnected
           (tail-degrees->graphs-stream (stream-rest gen-datas)))
          ((null? (car all-categories))
           (tail-degrees->graphs-stream
            (stream-cons (graph-gen-data data-edges degrees first-vertex (cdr all-categories)
                                         first-vertex-categories)
                         (stream-rest gen-datas))))
          ((null? (car first-vertex-categories))
           (tail-degrees->graphs-stream
            (stream-cons (graph-gen-data data-edges degrees first-vertex all-categories
                                         (cdr first-vertex-categories))
                         (stream-rest gen-datas))))
          ((memf (λ (deg) (>= deg length-degrees)) degrees); impossible condition => shortcut
           (tail-degrees->graphs-stream (stream-rest gen-datas)))
          (else
           (let* ((first-second-same-category (member (+ 1 first-vertex) (car all-categories)))
                  (new-all-categories (filter-out-vertex-from-categories first-vertex all-categories))
                  (new-v1-categories (filter-out-vertex-from-categories first-vertex first-vertex-categories))
                  (edge-categories (get-edge-categories first-vertex new-v1-categories))
                  (edgess (rec-parts-w/nb-categories-stream edge-categories (car degrees))))
             (tail-degrees->graphs-stream
              (stream-append
               (stream-map
                (λ (edges)
                  (get-edges-gen-data-stream data-edges edges degrees first-vertex new-all-categories
                                             new-v1-categories first-second-same-category))
                edgess)
               (stream-rest gen-datas)))))))))
                

(define (get-degrees-categories degrees first-vertex)
  (let* ((first-degree (car degrees))
         (index (index-where degrees (λ (d) (not (equal? d first-degree))))))
    (if index
        (cons (range first-vertex (+ first-vertex index))
              (get-degrees-categories (drop degrees index) (+ first-vertex index)))
        (list (range first-vertex (+ first-vertex (length degrees)))))))

(check-equal? (get-degrees-categories '(2 2 3 4 4 5) 0)
              '((0 1) (2) (3 4) (5)))

(define (degrees->graphs degrees)
  (define categories (get-degrees-categories degrees 0))
  (rec-degrees->graphs degrees 0 categories categories))

(define (degrees->graphs-stream degrees)
  (define categories (get-degrees-categories degrees 0))
  (tail-degrees->graphs-stream 
   (stream
    (graph-gen-data '() degrees 0 categories categories))))

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

(check-equal? (graph->degrees '((0 1)) 1) '(1 1))
(check-equal? (graph->degrees '((0 1) (0 2) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4)) 4) '(3 3 3 3 4))

(define (remove-vertex graph vertex)
  (filter (λ (edge) (not (member vertex edge))) graph))

(check-equal? (remove-vertex '((0 1) (0 2) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4)) 4)
              '((0 1) (0 2) (1 3) (2 3)))

(check-equal? (rec-degrees->graphs '(0 2) 3 '((4)) '((4))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream  '(0 2) 3 '((4)) '((4))))
              (rec-degrees->graphs '(0 2) 3 '((4)) '((4))))
(check-equal? (rec-degrees->graphs '(0) 1 '((1)) '((1))) '(()))
(check-equal? (stream->list (rec-degrees->graphs-stream  '(0) 1 '((1)) '((1))))
              (rec-degrees->graphs '(0) 1 '((1)) '((1))))(check-equal? (get-new-new-categories '((1)) '((0 1))) '((1)))
(check-equal? (get-new-degrees '((0 1)) '(1) 0) '(0))
(check-equal? (rec-parts-w/nb-categories '(((0 1))) 1) '(((0 1))))
(check-equal? (get-edge-categories 0 '((1))) '(((0 1))))
(check-equal? (filter-out-vertex-from-categories 0 '((0 1))) '((1)))

(check-equal? (degrees->graphs '(1 1)) '(((0 1)))) ; 0--1
(check-equal? (stream->list (degrees->graphs-stream '(1 1)))
              (degrees->graphs '(1 1)))
(check-equal? (stream->list (tail-degrees->graphs-stream
                             (stream (graph-gen-data '() '(1 1) 0 '((0 1)) '((0 1))))))
              (degrees->graphs '(1 1)))

(check-equal? (rec-degrees->graphs '(0) 3 '((3)) '((3))) '(()))
(check-equal? (stream->list (rec-degrees->graphs-stream  '(0) 3 '((3)) '((3))))
              (rec-degrees->graphs '(0) 3 '((3)) '((3))))

(check-equal? (get-new-new-categories '((3)) '((2 3))) '((3)))
(check-equal? (get-new-degrees '((2 3)) '(1) 2) '(0))
(check-equal? (rec-parts-w/nb-categories '(((2 3))) 1) '(((2 3))))
(check-equal? (get-edge-categories 2 '((3))) '(((2 3))))
(check-equal? (filter-out-vertex-from-categories 2 '((2 3))) '((3)))
(check-equal? (rec-degrees->graphs '(1 1) 2 '((2 3)) '((2 3))) '(((2 3))))
(check-equal? (stream->list (rec-degrees->graphs-stream  '(1 1) 2 '((2 3)) '((2 3))))
              (rec-degrees->graphs '(1 1) 2 '((2 3)) '((2 3))))

(check-equal? (get-new-new-categories '((2 3)) '((1 2) (1 3))) '((2 3)))
(check-equal? (get-new-degrees '((1 2) (1 3)) '(2 2) 1) '(1 1))
(check-equal? (rec-parts-w/nb-categories '(((1 2) (1 3))) 2) '(((1 2) (1 3))))
(check-equal? (get-edge-categories 1 '((2 3))) '(((1 2) (1 3))))
(check-equal? (filter-out-vertex-from-categories 1 '((1) (2 3))) '((2 3)))
(check-equal? (rec-degrees->graphs '(2 2 2) 1 '((1) (2 3)) '((1) (2 3))) '(((1 2) (1 3) (2 3))))
(check-equal? (stream->list (rec-degrees->graphs-stream  '(2 2 2) 1 '((1) (2 3)) '((1) (2 3))))
              (rec-degrees->graphs '(2 2 2) 1 '((1) (2 3)) '((1) (2 3))))

(check-equal? (get-new-new-categories '((1) (2 3)) '((0 2) (0 3))) '((1) (2 3)))
(check-equal? (get-new-degrees '((0 2) (0 3)) '(2 3 3) 0) '(2 2 2))

(check-equal? (rec-parts-w/nb-categories '() '(2)) '())
(check-equal? (get-edge-categories 3 '()) '())
(check-equal? (filter-out-vertex-from-categories 3 '((3))) '())
(check-equal? (rec-degrees->graphs '(2) 3 '((3)) '((3))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream  '(2) 3 '((3)) '((3))))
              (rec-degrees->graphs '(2) 3 '((3)) '((3))))

(check-equal? (get-new-new-categories '((3)) '((2 3))) '((3)))
(check-equal? (get-new-degrees '((2 3)) '(3) 2) '(2))
(check-equal? (rec-parts-w/nb-categories '(((2 3))) 1) '(((2 3))))
(check-equal? (get-edge-categories 2 '((3))) '(((2 3))))
(check-equal? (filter-out-vertex-from-categories 2 '((2) (3))) '((3)))
(check-equal? (rec-degrees->graphs '(1 3) 2 '((2) (3)) '((2) (3))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream  '(1 3) 2 '((2) (3)) '((2) (3))))
              (rec-degrees->graphs '(1 3) 2 '((2) (3)) '((2) (3))))

(check-equal? (get-new-new-categories '((2) (3)) '((1 2))) '((2) (3)))
(check-equal? (get-new-degrees '((1 2)) '(2 3) 1) '(1 3))
(check-equal? (rec-parts-w/nb-categories '(((1 2)) ((1 3))) 1) '(((1 2)) ((1 3))))
(check-equal? (get-edge-categories 1 '((2) (3))) '(((1 2)) ((1 3))))
(check-equal? (filter-out-vertex-from-categories 1 '((1) (2) (3))) '((2) (3)))
(check-equal? (rec-degrees->graphs '(1 2 3) 1 '((1) (2) (3)) '((1) (2) (3))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream   '(1 2 3) 1 '((1) (2) (3)) '((1) (2) (3))))
              (rec-degrees->graphs  '(1 2 3) 1 '((1) (2) (3)) '((1) (2) (3))))

(check-equal? (get-new-new-categories '((1) (2 3)) '((0 1) (0 2))) '((1) (2) (3)))
(check-equal? (get-new-degrees '((0 1) (0 2)) '(2 3 3) 0) '(1 2 3))
(check-equal? (rec-parts-w/nb-categories '(((0 1)) ((0 2) (0 3))) 2) '(((0 1) (0 2)) ((0 2) (0 3))))
(check-equal? (get-edge-categories 0 '((1) (2 3))) '(((0 1)) ((0 2) (0 3))))
(check-equal? (filter-out-vertex-from-categories 0 '((0 1) (2 3))) '((1) (2 3)))
(check-equal? (rec-degrees->graphs '(2 2 3 3) 0 '((0 1) (2 3)) '((0 1) (2 3))) '(((0 2) (0 3) (1 2) (1 3) (2 3))))
(check-equal? (stream->list (rec-degrees->graphs-stream  '(2 2 3 3) 0 '((0 1) (2 3)) '((0 1) (2 3))))
              (rec-degrees->graphs '(2 2 3 3) 0 '((0 1) (2 3)) '((0 1) (2 3))))


(check-equal? (rec-degrees->graphs '(1) 4 '((4)) '((4))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream  '(1) 4 '((4)) '((4))))
              (rec-degrees->graphs '(1) 4 '((4)) '((4))))
(check-equal? (get-new-new-categories '((4)) '([3 4])) '((4)))
(check-equal? (remove-categories-before-first-joined '((4)) '([3 4])) '((4)))
(check-equal? (get-new-degrees '([3 4]) '(2) 3) '(1))
(check-equal? (rec-parts-w/nb-categories '(([3 4])) 0) '(()))
(check-equal? (get-edge-categories 3 '((4))) '(([3 4])))
(check-equal? (filter-out-vertex-from-categories 3 '((4))) '((4)))
(check-equal? (rec-degrees->graphs '(0 2) 3 '((4)) '((4))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream '(0 2) 3 '((4)) '((4))))
              (rec-degrees->graphs '(0 2) 3 '((4)) '((4))))
(check-equal? (get-new-new-categories '((4)) '([2 3] [2 4])) '((4)))
(check-equal? (remove-categories-before-first-joined '((3) (4)) '([2 3] [2 4])) '((3) (4)))
(check-equal? (get-new-degrees '([2 3] [2 4]) '(1 3) 2) '(0 2))
(check-equal? (rec-parts-w/nb-categories  '(([2 3]) ([2 4])) 2) '(([2 3] [2 4])))
(check-equal? (get-edge-categories 2 '((3) (4))) '(([2 3]) ([2 4])))
(check-equal? (filter-out-vertex-from-categories 2 '((2) (3) (4))) '((3) (4)))
(check-equal? (rec-degrees->graphs '(2 1 3) 2 '((2) (3) (4)) '((2) (3) (4))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 1 3) 2 '((2) (3) (4)) '((2) (3) (4))))
              (rec-degrees->graphs '(2 1 3) 2 '((2) (3) (4)) '((2) (3) (4))))
(check-equal? (get-new-new-categories '((3) (4)) '([1 3])) '((3) (4)))
(check-equal? (remove-categories-before-first-joined '((2) (3) (4)) '([1 3])) '((3) (4)))
(check-equal? (get-new-degrees '([1 3]) '(2 2 3) 1) '(2 1 3))
              
(check-equal? (rec-degrees->graphs '(1 2 3) 2 '((2) (3) (4)) '((2) (3) (4))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream '(1 2 3) 2 '((2) (3) (4)) '((2) (3) (4))))
              (rec-degrees->graphs '(1 2 3) 2 '((2) (3) (4)) '((2) (3) (4))))
(check-equal? (get-new-new-categories '((2) (3) (4)) '([1 2])) '((2) (3) (4)))
(check-equal? (remove-categories-before-first-joined '((2) (3) (4)) '([1 2])) '((2) (3) (4)))
(check-equal? (get-new-degrees '([1 2]) '(2 2 3) 1) '(1 2 3))

(check-equal? (rec-parts-w/nb-categories  '(([1 2]) ([1 3]) ([1 4])) 1) '(([1 2]) ([1 3]) ([1 4])))
(check-equal? (get-edge-categories 1  '((2) (3) (4)))  '(([1 2]) ([1 3]) ([1 4])))
(check-equal? (filter-out-vertex-from-categories 1 '((1) (2) (3) (4))) '((2) (3) (4)))

(check-equal? (rec-degrees->graphs '(2 2 2) 2 '((2 3) (4)) '((4))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 2) 2 '((2 3) (4)) '((4))))
              (rec-degrees->graphs '(2 2 2) 2 '((2 3) (4)) '((4))))

(check-equal? (rec-degrees->graphs '(1 2 2 3) 1 '((1) (2) (3) (4)) '((1) (2) (3) (4))) '({(1 4) (2 3) (2 4) (3 4)}))
(check-equal? (stream->list (rec-degrees->graphs-stream '(1 2 2 3) 1 '((1) (2) (3) (4)) '((1) (2) (3) (4))))
              (rec-degrees->graphs '(1 2 2 3) 1 '((1) (2) (3) (4)) '((1) (2) (3) (4))))

(check-equal? (rec-degrees->graphs '(1 2 2 3) 1 '((1) (2 3) (4)) '((1) (2 3) (4))) '({(1 4) (2 3) (2 4) (3 4)}))
(check-equal? (stream->list (rec-degrees->graphs-stream '(1 2 2 3) 1 '((1) (2 3) (4)) '((1) (2 3) (4))))
              (rec-degrees->graphs '(1 2 2 3) 1 '((1) (2 3) (4)) '((1) (2 3) (4))))

(check-equal? (get-new-new-categories '((1 2) (3 4)) '([0 1] [0 3])) '((1) (2) (3) (4)))
(check-equal? (get-new-degrees '([0 1] [0 3]) '(2 2 3 3) 0) '(1 2 2 3))
            
(check-equal? (rec-degrees->graphs '(1 1 3 3) 1 '((1 2) (3 4)) '((1 2) (3 4))) '())
(check-equal? (get-new-new-categories '((1 2) (3 4)) '([0 1] [0 2])) '((1 2) (3 4)))
(check-equal? (get-new-degrees '([0 1] [0 2]) '(2 2 3 3) 0) '(1 1 3 3))

(check-equal? (rec-parts-w/nb-categories '(([0 1] [0 2]) ([0 3] [0 4])) 2)
              '(([0 1] [0 2]) ([0 1] [0 3]) ([0 3] [0 4])))
(check-equal? (get-edge-categories 0 '((1 2) (3 4))) '(([0 1] [0 2]) ([0 3] [0 4])))
(check-equal? (filter-out-vertex-from-categories 0 '((0 1 2) (3 4))) '((1 2) (3 4)))


;'((0 3) (0 4) (1 2) (1 3) (2 4) (3 4)) should not be returned,
; as similar to '((0 1) (0 3) (1 4) (2 3) (2 4) (3 4)).
; There should not be a degree 2 vertex edging to another degree 2 vertex
; after another degree 2 vertex in same category edging to only degree 3 vertices.
;
; 0   2   0   1
; |\ /|   |\ /|
; 1 3 |   | 3 2
; \ | /   \ | /
;  \|/     \|/
;   4       4
(check-equal? (rec-degrees->graphs '(2 2 2 3 3) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4)))
              '(((0 1) (0 3) (1 4) (2 3) (2 4) (3 4))
                ((0 3) (0 4) (1 3) (1 4) (2 3) (2 4))))
(check-equal? (stream->list (degrees->graphs-stream '(2 2 2 3 3)))
              (rec-degrees->graphs '(2 2 2 3 3) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4))))
(check-equal? (stream->list (tail-degrees->graphs-stream
                             (stream (graph-gen-data '() '(2 2 2 3 3) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4))))))
              (degrees->graphs '(2 2 2 3 3)))

; 0--1--3--4--2
; `-----------'
(check-equal? (rec-degrees->graphs '(2 2 2 2 2) 0 '((0 1 2 3 4)) '((0 1 2 3 4)))
              '(((0 1) (0 2) (1 3) (2 4) (3 4))))
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 2 2 2) 0 '((0 1 2 3 4)) '((0 1 2 3 4))))
              (rec-degrees->graphs '(2 2 2 2 2) 0 '((0 1 2 3 4)) '((0 1 2 3 4))))

; 0   2
; |\ /|
; | 4 |
; |/ \|
; 1   3
(check-equal? (rec-degrees->graphs '(2 2 2 2 4) 0 '((0 1 2 3) (4)) '((0 1 2 3) (4)))
              '(((0 1) (0 4) (1 4) (2 3) (2 4) (3 4))))
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 2 2 4) 0 '((0 1 2 3) (4)) '((0 1 2 3) (4))))
              (rec-degrees->graphs '(2 2 2 2 4) 0 '((0 1 2 3) (4)) '((0 1 2 3) (4))))


; 0--3--2
;  \/| /
;  /\|/
; 1--4
(check-equal? (rec-degrees->graphs '(2 2 2 4 4) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4)))
              '(((0 3) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4))))
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 2 4 4) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4))))
              (rec-degrees->graphs '(2 2 2 4 4) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4))))


; 0--2---3--1
; |   \ /   |
; `----4----'
(check-equal? (rec-degrees->graphs '(2 2 3 3 4) 0 '((0 1) (2 3) (4)) '((0 1) (2 3) (4)))
              '(((0 2) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4))))
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 3 3 4) 0 '((0 1) (2 3) (4)) '((0 1) (2 3) (4))))
              (rec-degrees->graphs '(2 2 3 3 4) 0 '((0 1) (2 3) (4)) '((0 1) (2 3) (4))))

; 0
; |\
; 1 2---3
; |  \ /
; `---4
(check-equal? (rec-degrees->graphs '(2 3 3 3 3) 0 '((0) (1 2 3 4)) '((0) (1 2 3 4)))
              '(((0 1) (0 2) (1 3) (1 4) (2 3) (2 4) (3 4))))
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 3 3 3 3) 0 '((0) (1 2 3 4)) '((0) (1 2 3 4))))
              (rec-degrees->graphs '(2 3 3 3 3) 0 '((0) (1 2 3 4)) '((0) (1 2 3 4))))

(check-equal? (rec-degrees->graphs '(2 2 4 4 4) 0 '((0 1) (2 3 4)) '((0 1) (2 3 4)))
              '())
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 4 4 4) 0 '((0 1) (2 3 4)) '((0 1) (2 3 4))))
              (rec-degrees->graphs '(2 2 4 4 4) 0 '((0 1) (2 3 4)) '((0 1) (2 3 4))))

;     0
;    / \
;   3---4
;   |\ /|
;   | X |
;   |/ \|
;   1---2
(check-equal? (rec-degrees->graphs '(2 3 3 4 4) 0 '((0) (1 2) (3 4)) '((0) (1 2) (3 4)))
              '(((0 3) (0 4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4))))
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 3 3 4 4) 0 '((0) (1 2) (3 4)) '((0) (1 2) (3 4))))
              (rec-degrees->graphs '(2 3 3 4 4) 0 '((0) (1 2) (3 4)) '((0) (1 2) (3 4))))

; ,---------.
; 0--2---3--1
; |   \ /   |
; `----4----'
(check-equal? (degrees->graphs '(3 3 3 3 4))
              '(((0 1) (0 2) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4))))
(check-equal? (stream->list (rec-degrees->graphs-stream '(3 3 3 3 4) 0 '((0 1 2 3) (4)) '((0 1 2 3) (4))))
              (degrees->graphs '(3 3 3 3 4)))

(check-equal? (rec-degrees->graphs '(2 4 4 4 4) 0 '((0) (1 2 3 4)) '((0) (1 2 3 4)))
              '())
(check-equal? (stream->list (degrees->graphs-stream '(2 4 4 4 4)))
              (rec-degrees->graphs '(2 4 4 4 4) 0 '((0) (1 2 3 4)) '((0) (1 2 3 4))))

;'((0 1) (0 2) (1 2) (3 4) (3 5) (4 5)) should not be returned, as disconnected
(check-equal? (degrees->graphs '(2 2 2 2 2 2)) '(((0 1) (0 2) (1 3) (2 4) (3 5) (4 5))))
(check-equal? (stream->list (degrees->graphs-stream  '(2 2 2 2 2 2)))
              (degrees->graphs '(2 2 2 2 2 2)))
(check-equal?
 (stream->list
  (tail-degrees->graphs-stream
   (stream (graph-gen-data '() '(2 2 2 2 2 2) 0 '((0 1 2 3 4 5)) '((0 1 2 3 4 5))))))
 '(((0 1) (0 2) (1 3) (2 4) (3 5) (4 5))))
                                                                
 



















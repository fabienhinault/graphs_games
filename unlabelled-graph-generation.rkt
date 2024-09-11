#lang racket


(require srfi/67) ; compare procedures
(require "graph-utils.rkt")
(require "combinatorics.rkt")
(require "graph-generate.rkt")

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

(define (rename-numeric-graph graph new-names)
  (map (λ (e) (rename-numeric-edge e new-names))
       graph))

(define (rename-numeric-edge edge new-names)
  (map (λ (v) (rename-numeric-vertex v new-names))
       edge))

(define (rename-numeric-vertex vertex new-names)
  (list-ref new-names vertex))

(module+ test
  (define
    G '((0 1) (2 3) (4 5) (6 7) (6 11) (8 9) (10 9) (12 13) (14 13) (14 15) (16 17) (18 15) (0 20) (1 19)
              (2 19) (4 20) (8 22) (10 22) (12 21) (16 20) (18 19) (3 23) (5 23) (7 23) (5 27) (7 11)
              (24 25) (24 26) (3 21) (24 20) (25 27) (26 27) (28 29) (9 15) (25 13) (27 13) (28 17)
              (29 13) (29 15) (29 17) (9 21) (11 21) (11 22) (25 22) (26 21) (26 22) (28 20) (28 21)
              (15 17) (17 19) (19 20) (19 22)))
  (check-equal?
   (rename-numeric-graph
    G
    '(0 1 2 4 6 8 10 12 14 16 18 23 3 5 7 24 27 25 26 28 29 9 11 17 15 13 19 20 22 21))
   '((0 1) (2 4) (6 8) (10 12) (10 23) (14 16) (18 16) (3 5) (7 5) (7 24) (27 25) (26 24) (0 29)
           (1 28) (2 28) (6 29) (14 11) (18 11) (3 9) (27 29) (26 28) (4 17) (8 17) (12 17) (8 20)
           (12 23) (15 13) (15 19) (4 9) (15 29) (13 20) (19 20) (22 21) (16 24) (13 5) (20 5)
           (22 25) (21 5) (21 24) (21 25) (16 9) (23 9) (23 11) (13 11) (19 9) (19 11) (22 29)
           (22 9) (24 25) (25 28) (28 29) (28 11))))

(define (sort-graph graph)
  (define sorted-vertices (map (λ (edge) (sort edge <)) graph))
  (sort sorted-vertices <?))
  
(module+ test
  (check-equal?
   (sort-graph
    '((0 1) (2 4) (6 8) (10 12) (10 23) (14 16) (18 16) (3 5) (7 5) (7 24) (27 25) (26 24) (0 29)
            (1 28) (2 28) (6 29) (14 11) (18 11) (3 9) (27 29) (26 28) (4 17) (8 17) (12 17) (8 20)
            (12 23) (15 13) (15 19) (4 9) (15 29) (13 20) (19 20) (22 21) (16 24) (13 5) (20 5)
            (22 25) (21 5) (21 24) (21 25) (16 9) (23 9) (23 11) (13 11) (19 9) (19 11) (22 29)
            (22 9) (24 25) (25 28) (28 29) (28 11)))
   '((0 1) (0 29) (1 28) (2 4) (2 28) (3 5) (3 9) (4 9) (4 17) (5 7) (5 13) (5 20) (5 21) (6 8)
           (6 29) (7 24) (8 17) (8 20) (9 16) (9 19) (9 22) (9 23) (10 12) (10 23) (11 13) (11 14)
           (11 18) (11 19) (11 23) (11 28) (12 17) (12 23) (13 15) (13 20) (14 16) (15 19) (15 29)
           (16 18) (16 24) (19 20) (21 22) (21 24) (21 25) (22 25) (22 29) (24 25) (24 26) (25 27)
           (25 28) (26 28) (27 29) (28 29))))
  
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

(define (get-neighbours-min-degree vertex graph forbiddens get-vertex-degree)
  (define vertex-degree (get-vertex-degree vertex graph))
  (define neighbours-greater-degree
    (filter (λ (v) (<= vertex-degree (get-vertex-degree v graph))) (get-neighbours vertex graph forbiddens)))
  (min&args neighbours-greater-degree (λ (v) (get-vertex-degree v graph))))

(define (vertex<?* graph get-vertex-degree)
  (λ (v1 v2)
    (let* ((d1 (get-vertex-degree v1 graph))
           (d2 (get-vertex-degree v2 graph)))
      (cond
        ((< d1 d2) #t)
        ((< d2 d1) #f)
        (else
         (let* ((n1 (get-neighbours-min-degree v1 graph (list v2) get-vertex-degree))
                (n2 (get-neighbours-min-degree v2 graph (list v1) get-vertex-degree)))
           (< (car n1) (car n2))))))))

(module+ test
  (let ((v< (vertex<?* G get-degree)))
    (check-false (v< 0 1))
    (check-false (v< 1 0))
    (check-true  (v< 1 2))
  (check-equal? (sort (range 30) v<)
                '(0 1 2 4 6 8 10 12 14 16 18 23 3 5 7 24 27 25 26 28 29 9 11 17 15 13 19 20 22 21))))

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



(define (get-new-edgess n)
  (let* ((new-node (- n 1))
         (old-nodes (range (- n 1)))
         (new-edges (map (λ (_) (list _ new-node)) old-nodes)))
    ; remove '() at start
    (cdr (tailrec-parts '(()) new-edges))))

(module+ test
  (check-equal? (get-new-edgess 0) '())
  (check-equal? (get-new-edgess 1) '())
  (check-equal? (get-new-edgess 2) '(((0 1))))
  (check-equal? (get-new-edgess 3) '(((0 2)) ((1 2)) ((1 2) (0 2))))
  (check-equal? (get-new-edgess 4)
                '(((0 3)) ((1 3))
                          ((1 3) (0 3)) 
                          ((2 3))
                          ((2 3) (0 3)) ((2 3) (1 3))
                          ((2 3) (1 3) (0 3)))))


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

(module+ test
  (check-equal? (new-graphs '() (get-new-edgess 2) 2) '(((0 1)))))


(define (new-graph old-graph new-edges)
  ; need to rewrite twice to reorder vertices in edges after rename
  (rewrite-graph
   (rec-rename-graph-vertices (rewrite-graph (append old-graph new-edges))
                              (make-hash) (new-name-numeric-generator))))


(module+ test
  ;                          (1 0) (3 2) (0 2)    (1 4) (2 4) (3 4)
  (check-equal? (new-graph '((0 1) (2 3) (1 3)) '((0 4) (2 4) (3 4)))
                '((0 1) (0 2) (1 3) (4 2) (4 3) (2 3)))

  (check-equal? (new-graph '((0 1) (2 3) (1 3)) '((0 4) (1 4) (2 4)))
                '((0 1) (0 4) (1 3) (2 3) (2 4) (3 4))))



(define (generate-unlabeled-n-graph-from-sub1n nb-vertices graphs-sub1n)
  (let ((new-edges (get-new-edgess nb-vertices)))
    (remove-duplicates (apply append (map (λ (_) (new-graphs _ new-edges nb-vertices)) graphs-sub1n)))))


(module+ test
  (check-equal? (generate-unlabeled-n-graph-from-sub1n 2 '(())) '(((0 1))))
  (check-equal? (generate-unlabeled-n-graph-from-sub1n 3 '(((0 1))))
                '(((0 2) (1 2)) ((0 1) (0 2) (1 2))))
  (check-equal? (generate-unlabeled-n-graph-from-sub1n 4 '(((0 1) (2 1)) ((0 1) (0 2) (1 2))))
                '(((0 2) (1 3) (2 3))
                  ((0 3) (1 3) (2 3))
                  ((0 3) (1 2) (1 3) (2 3))
                  ((0 1) (0 2) (1 3) (2 3))
                  ((0 2) (0 3) (1 2) (1 3) (2 3))
                  ((0 1) (0 2) (0 3) (1 2) (1 3) (2 3)))))


(define graphs-by-node-nb (make-vector 100))
(vector-set! graphs-by-node-nb 1 '(()))
(for ((i-node-nb (range 2 7)))
  (vector-set! graphs-by-node-nb i-node-nb
               (generate-unlabeled-n-graph-from-sub1n
                i-node-nb
                (vector-ref graphs-by-node-nb (- i-node-nb 1)))))


; all growing suites of nb-vertices integers between min-degree and max-degree
; whose sum is total-degree
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

(module+ test
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
  (check-equal? (degreess 6 30 2 5) '((5 5 5 5 5 5))))


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

(define (get-edges-subgraphs edges degrees first-vertex new-all-categories
                             new-v1-categories first-second-same-category
                             first-already-used new-used-vertices)
  (define new-degrees (get-new-degrees edges (cdr degrees) first-vertex))
  (define new-new-all-categories (get-new-new-categories new-all-categories edges))
  (define new-new-v1-categories 
    (if (and first-second-same-category (not first-already-used))
        (get-new-new-categories
         (remove-categories-before-first-joined new-v1-categories edges)
         edges)
        new-new-all-categories))
  (define sub-graphs (rec-degrees->graphs new-degrees
                                      (+ 1 first-vertex)
                                      new-new-all-categories
                                      new-new-v1-categories
                                      new-used-vertices))
  (if (null? sub-graphs)
      '()
      (map (λ (sub-graph)
             (append edges sub-graph))
           sub-graphs)))

;@defproc[(rec-degrees->graphs [degrees (listof(integer?))] first-vertex all-categories first-vertex-categories)
;         (listof(listof integer?))]{
;      Returns list of graphs representing unlabelled graphs satisfying the constraints of degrees
;              }
                                                                                                                 

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
(define (rec-degrees->graphs degrees first-vertex all-categories first-vertex-categories used-vertices)
  (define length-degrees (length degrees))
  (cond ((equal? degrees '())
         '());break
        ((equal? degrees '(0))
         '(())) ; the empty graph
        ((and (equal? 0 (car degrees)) (memf (λ (deg) (> deg 0)) degrees))
         '()) ; break as leading to disconnected graphs
        ((null? (car all-categories))
         (rec-degrees->graphs degrees first-vertex (cdr all-categories) first-vertex-categories used-vertices))
        ((null? (car first-vertex-categories))
         (rec-degrees->graphs degrees first-vertex all-categories (cdr first-vertex-categories used-vertices)))
        ((memf (λ (deg) (>= deg length-degrees)) degrees)
         '());break
        (else (let* ((first-second-same-category (member (+ 1 first-vertex) (car all-categories)))
                     (first-already-used (set-member? used-vertices first-vertex))
                     (new-all-categories (filter-out-vertex-from-categories first-vertex all-categories))
                     (new-v1-categories (filter-out-vertex-from-categories first-vertex first-vertex-categories))
                     (new-used-vertices (set-remove used-vertices first-vertex))
                     (edge-categories (get-edge-categories first-vertex new-v1-categories))
                     (edgess (rec-parts-w/nb-categories edge-categories (car degrees))))
                 (append-map
                  (λ (edges)
                    (get-edges-subgraphs edges degrees first-vertex new-all-categories
                                         new-v1-categories first-second-same-category
                                         first-already-used new-used-vertices))
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
                #f))

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

(module+ test
  (check-equal? (get-degrees-categories '(2 2 3 4 4 5) 0)
                '((0 1) (2) (3 4) (5))))

(define (degrees->graphs degrees)
  (define categories (get-degrees-categories degrees 0))
  (rec-degrees->graphs degrees 0 categories categories (set)))

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

(module+ test
  (check-equal? (graph->degrees '((0 1)) 1) '(1 1))
  (check-equal? (graph->degrees '((0 1) (0 2) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4)) 4) '(3 3 3 3 4))

  (define (remove-vertex graph vertex)
    (filter (λ (edge) (not (member vertex edge))) graph))

  (check-equal? (remove-vertex '((0 1) (0 2) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4)) 4)
                '((0 1) (0 2) (1 3) (2 3)))

  (check-equal? (rec-degrees->graphs '(0 2) 3 '((4)) '((4)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream  '(0 2) 3 '((4)) '((4))))
                (rec-degrees->graphs '(0 2) 3 '((4)) '((4)) (set)))
  (check-equal? (rec-degrees->graphs '(0) 1 '((1)) '((1)) (set)) '(()))
  (check-equal? (stream->list (rec-degrees->graphs-stream  '(0) 1 '((1)) '((1))))
                (rec-degrees->graphs '(0) 1 '((1)) '((1)) (set)))
  (check-equal? (get-new-new-categories '((1)) '((0 1))) '((1)))
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

  (check-equal? (rec-degrees->graphs '(0) 3 '((3)) '((3)) (set)) '(()))
  (check-equal? (stream->list (rec-degrees->graphs-stream  '(0) 3 '((3)) '((3))))
                (rec-degrees->graphs '(0) 3 '((3)) '((3)) (set)))

  (check-equal? (get-new-new-categories '((3)) '((2 3))) '((3)))
  (check-equal? (get-new-degrees '((2 3)) '(1) 2) '(0))
  (check-equal? (rec-parts-w/nb-categories '(((2 3))) 1) '(((2 3))))
  (check-equal? (get-edge-categories 2 '((3))) '(((2 3))))
  (check-equal? (filter-out-vertex-from-categories 2 '((2 3))) '((3)))
  (check-equal? (rec-degrees->graphs '(1 1) 2 '((2 3)) '((2 3)) (set)) '(((2 3))))
  (check-equal? (stream->list (rec-degrees->graphs-stream  '(1 1) 2 '((2 3)) '((2 3))))
                (rec-degrees->graphs '(1 1) 2 '((2 3)) '((2 3)) (set)))

  (check-equal? (get-new-new-categories '((2 3)) '((1 2) (1 3))) '((2 3)))
  (check-equal? (get-new-degrees '((1 2) (1 3)) '(2 2) 1) '(1 1))
  (check-equal? (rec-parts-w/nb-categories '(((1 2) (1 3))) 2) '(((1 2) (1 3))))
  (check-equal? (get-edge-categories 1 '((2 3))) '(((1 2) (1 3))))
  (check-equal? (filter-out-vertex-from-categories 1 '((1) (2 3))) '((2 3)))
  (check-equal? (rec-degrees->graphs '(2 2 2) 1 '((1) (2 3)) '((1) (2 3)) (set)) '(((1 2) (1 3) (2 3))))
  (check-equal? (stream->list (rec-degrees->graphs-stream  '(2 2 2) 1 '((1) (2 3)) '((1) (2 3))))
                (rec-degrees->graphs '(2 2 2) 1 '((1) (2 3)) '((1) (2 3)) (set)))

  (check-equal? (get-new-new-categories '((1) (2 3)) '((0 2) (0 3))) '((1) (2 3)))
  (check-equal? (get-new-degrees '((0 2) (0 3)) '(2 3 3) 0) '(2 2 2))

  (check-equal? (rec-parts-w/nb-categories '() '(2)) '())
  (check-equal? (get-edge-categories 3 '()) '())
  (check-equal? (filter-out-vertex-from-categories 3 '((3))) '())
  (check-equal? (rec-degrees->graphs '(2) 3 '((3)) '((3)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream  '(2) 3 '((3)) '((3))))
                (rec-degrees->graphs '(2) 3 '((3)) '((3)) (set)))

  (check-equal? (get-new-new-categories '((3)) '((2 3))) '((3)))
  (check-equal? (get-new-degrees '((2 3)) '(3) 2) '(2))
  (check-equal? (rec-parts-w/nb-categories '(((2 3))) 1) '(((2 3))))
  (check-equal? (get-edge-categories 2 '((3))) '(((2 3))))
  (check-equal? (filter-out-vertex-from-categories 2 '((2) (3))) '((3)))
  (check-equal? (rec-degrees->graphs '(1 3) 2 '((2) (3)) '((2) (3)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream  '(1 3) 2 '((2) (3)) '((2) (3))))
                (rec-degrees->graphs '(1 3) 2 '((2) (3)) '((2) (3)) (set)))

  (check-equal? (get-new-new-categories '((2) (3)) '((1 2))) '((2) (3)))
  (check-equal? (get-new-degrees '((1 2)) '(2 3) 1) '(1 3))
  (check-equal? (rec-parts-w/nb-categories '(((1 2)) ((1 3))) 1) '(((1 2)) ((1 3))))
  (check-equal? (get-edge-categories 1 '((2) (3))) '(((1 2)) ((1 3))))
  (check-equal? (filter-out-vertex-from-categories 1 '((1) (2) (3))) '((2) (3)))
  (check-equal? (rec-degrees->graphs '(1 2 3) 1 '((1) (2) (3)) '((1) (2) (3)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream   '(1 2 3) 1 '((1) (2) (3)) '((1) (2) (3))))
                (rec-degrees->graphs  '(1 2 3) 1 '((1) (2) (3)) '((1) (2) (3)) (set)))

  (check-equal? (get-new-new-categories '((1) (2 3)) '((0 1) (0 2))) '((1) (2) (3)))
  (check-equal? (get-new-degrees '((0 1) (0 2)) '(2 3 3) 0) '(1 2 3))
  (check-equal? (rec-parts-w/nb-categories '(((0 1)) ((0 2) (0 3))) 2) '(((0 1) (0 2)) ((0 2) (0 3))))
  (check-equal? (get-edge-categories 0 '((1) (2 3))) '(((0 1)) ((0 2) (0 3))))
  (check-equal? (filter-out-vertex-from-categories 0 '((0 1) (2 3))) '((1) (2 3)))
  (check-equal? (rec-degrees->graphs '(2 2 3 3) 0 '((0 1) (2 3)) '((0 1) (2 3)) (set)) '(((0 2) (0 3) (1 2) (1 3) (2 3))))
  (check-equal? (stream->list (rec-degrees->graphs-stream  '(2 2 3 3) 0 '((0 1) (2 3)) '((0 1) (2 3))))
                (rec-degrees->graphs '(2 2 3 3) 0 '((0 1) (2 3)) '((0 1) (2 3)) (set)))


  (check-equal? (rec-degrees->graphs '(1) 4 '((4)) '((4)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream  '(1) 4 '((4)) '((4))))
                (rec-degrees->graphs '(1) 4 '((4)) '((4)) (set)))
  (check-equal? (get-new-new-categories '((4)) '([3 4])) '((4)))
  (check-equal? (remove-categories-before-first-joined '((4)) '([3 4])) '((4)))
  (check-equal? (get-new-degrees '([3 4]) '(2) 3) '(1))
  (check-equal? (rec-parts-w/nb-categories '(([3 4])) 0) '(()))
  (check-equal? (get-edge-categories 3 '((4))) '(([3 4])))
  (check-equal? (filter-out-vertex-from-categories 3 '((4))) '((4)))
  (check-equal? (rec-degrees->graphs '(0 2) 3 '((4)) '((4)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream '(0 2) 3 '((4)) '((4))))
                (rec-degrees->graphs '(0 2) 3 '((4)) '((4)) (set)))
  (check-equal? (get-new-new-categories '((4)) '([2 3] [2 4])) '((4)))
  (check-equal? (remove-categories-before-first-joined '((3) (4)) '([2 3] [2 4])) '((3) (4)))
  (check-equal? (get-new-degrees '([2 3] [2 4]) '(1 3) 2) '(0 2))
  (check-equal? (rec-parts-w/nb-categories  '(([2 3]) ([2 4])) 2) '(([2 3] [2 4])))
  (check-equal? (get-edge-categories 2 '((3) (4))) '(([2 3]) ([2 4])))
  (check-equal? (filter-out-vertex-from-categories 2 '((2) (3) (4))) '((3) (4)))
  (check-equal? (rec-degrees->graphs '(2 1 3) 2 '((2) (3) (4)) '((2) (3) (4)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream '(2 1 3) 2 '((2) (3) (4)) '((2) (3) (4))))
                (rec-degrees->graphs '(2 1 3) 2 '((2) (3) (4)) '((2) (3) (4)) (set)))
  (check-equal? (get-new-new-categories '((3) (4)) '([1 3])) '((3) (4)))
  (check-equal? (remove-categories-before-first-joined '((2) (3) (4)) '([1 3])) '((3) (4)))
  (check-equal? (get-new-degrees '([1 3]) '(2 2 3) 1) '(2 1 3))
              
  (check-equal? (rec-degrees->graphs '(1 2 3) 2 '((2) (3) (4)) '((2) (3) (4)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream '(1 2 3) 2 '((2) (3) (4)) '((2) (3) (4))))
                (rec-degrees->graphs '(1 2 3) 2 '((2) (3) (4)) '((2) (3) (4)) (set)))
  (check-equal? (get-new-new-categories '((2) (3) (4)) '([1 2])) '((2) (3) (4)))
  (check-equal? (remove-categories-before-first-joined '((2) (3) (4)) '([1 2])) '((2) (3) (4)))
  (check-equal? (get-new-degrees '([1 2]) '(2 2 3) 1) '(1 2 3))

  (check-equal? (rec-parts-w/nb-categories  '(([1 2]) ([1 3]) ([1 4])) 1) '(([1 2]) ([1 3]) ([1 4])))
  (check-equal? (get-edge-categories 1  '((2) (3) (4)))  '(([1 2]) ([1 3]) ([1 4])))
  (check-equal? (filter-out-vertex-from-categories 1 '((1) (2) (3) (4))) '((2) (3) (4)))

  (check-equal? (rec-degrees->graphs '(2 2 2) 2 '((2 3) (4)) '((4)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 2) 2 '((2 3) (4)) '((4))))
                (rec-degrees->graphs '(2 2 2) 2 '((2 3) (4)) '((4)) (set)))

  (check-equal? (rec-degrees->graphs '(1 2 2 3) 1 '((1) (2) (3) (4)) '((1) (2) (3) (4)) (set)) '({(1 4) (2 3) (2 4) (3 4)}))
  (check-equal? (stream->list (rec-degrees->graphs-stream '(1 2 2 3) 1 '((1) (2) (3) (4)) '((1) (2) (3) (4))))
                (rec-degrees->graphs '(1 2 2 3) 1 '((1) (2) (3) (4)) '((1) (2) (3) (4)) (set)))

  (check-equal? (rec-degrees->graphs '(1 2 2 3) 1 '((1) (2 3) (4)) '((1) (2 3) (4)) (set)) '({(1 4) (2 3) (2 4) (3 4)}))
  (check-equal? (stream->list (rec-degrees->graphs-stream '(1 2 2 3) 1 '((1) (2 3) (4)) '((1) (2 3) (4))))
                (rec-degrees->graphs '(1 2 2 3) 1 '((1) (2 3) (4)) '((1) (2 3) (4)) (set)))

  (check-equal? (get-new-new-categories '((1 2) (3 4)) '([0 1] [0 3])) '((1) (2) (3) (4)))
  (check-equal? (get-new-degrees '([0 1] [0 3]) '(2 2 3 3) 0) '(1 2 2 3))
            
  (check-equal? (rec-degrees->graphs '(1 1 3 3) 1 '((1 2) (3 4)) '((1 2) (3 4)) (set)) '())
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
  (check-equal? (degrees->graphs '(2 2 2 3 3))
                '(((0 1) (0 3) (1 4) (2 3) (2 4) (3 4))
                  ((0 3) (0 4) (1 3) (1 4) (2 3) (2 4))))
  (check-equal? (stream->list (degrees->graphs-stream '(2 2 2 3 3)))
                (degrees->graphs '(2 2 2 3 3)))
  (check-equal? (stream->list (tail-degrees->graphs-stream
                               (stream (graph-gen-data '() '(2 2 2 3 3) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4))))))
                (degrees->graphs '(2 2 2 3 3)))

  ; 0--1--3--4--2
  ; `-----------'
  (check-equal? (degrees->graphs '(2 2 2 2 2))
                '(((0 1) (0 2) (1 3) (2 4) (3 4))))
  (check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 2 2 2) 0 '((0 1 2 3 4)) '((0 1 2 3 4))))
                (degrees->graphs '(2 2 2 2 2)))

  ; 0   2
  ; |\ /|
  ; | 4 |
  ; |/ \|
  ; 1   3
  (check-equal? (degrees->graphs '(2 2 2 2 4))
                '(((0 1) (0 4) (1 4) (2 3) (2 4) (3 4))))
  (check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 2 2 4) 0 '((0 1 2 3) (4)) '((0 1 2 3) (4))))
                (degrees->graphs '(2 2 2 2 4)))


  ; 0--3--2
  ;  \/| /
  ;  /\|/
  ; 1--4
  (check-equal? (degrees->graphs '(2 2 2 4 4))
                '(((0 3) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4))))
  (check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 2 4 4) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4))))
                (degrees->graphs '(2 2 2 4 4)))


  ; 0--2---3--1
  ; |   \ /   |
  ; `----4----'
  (check-equal? (degrees->graphs '(2 2 3 3 4))
                '(((0 2) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4))))
  (check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 3 3 4) 0 '((0 1) (2 3) (4)) '((0 1) (2 3) (4))))
                (degrees->graphs '(2 2 3 3 4)))

  ; 0
  ; |\
  ; 1 2---3
  ; |  \ /
  ; `---4
  (check-equal? (degrees->graphs '(2 3 3 3 3))
                '(((0 1) (0 2) (1 3) (1 4) (2 3) (2 4) (3 4))))
  (check-equal? (stream->list (rec-degrees->graphs-stream '(2 3 3 3 3) 0 '((0) (1 2 3 4)) '((0) (1 2 3 4))))
                (degrees->graphs '(2 3 3 3 3)))

  (check-equal? (degrees->graphs '(2 2 4 4 4))
                '())
  (check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 4 4 4) 0 '((0 1) (2 3 4)) '((0 1) (2 3 4))))
                (degrees->graphs '(2 2 4 4 4)))

  ;     0
  ;    / \
  ;   3---4
  ;   |\ /|
  ;   | X |
  ;   |/ \|
  ;   1---2
  (check-equal? (degrees->graphs '(2 3 3 4 4))
                '(((0 3) (0 4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4))))
  (check-equal? (stream->list (rec-degrees->graphs-stream '(2 3 3 4 4) 0 '((0) (1 2) (3 4)) '((0) (1 2) (3 4))))
                (degrees->graphs '(2 3 3 4 4)))

  ; ,---------.
  ; 0--2---3--1
  ; |   \ /   |
  ; `----4----'
  (check-equal? (degrees->graphs '(3 3 3 3 4))
                '(((0 1) (0 2) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4))))
  (check-equal? (stream->list (rec-degrees->graphs-stream '(3 3 3 3 4) 0 '((0 1 2 3) (4)) '((0 1 2 3) (4))))
                (degrees->graphs '(3 3 3 3 4)))

  (check-equal? (degrees->graphs '(2 4 4 4 4))
                '())
  (check-equal? (stream->list (degrees->graphs-stream '(2 4 4 4 4)))
                (degrees->graphs '(2 4 4 4 4)))

  ;'((0 1) (0 2) (1 2) (3 4) (3 5) (4 5)) should not be returned, as disconnected
  (check-equal? (degrees->graphs '(2 2 2 2 2 2)) '(((0 1) (0 2) (1 3) (2 4) (3 5) (4 5))))
  (check-equal? (stream->list (degrees->graphs-stream  '(2 2 2 2 2 2)))
                (degrees->graphs '(2 2 2 2 2 2)))
  (check-equal?
   (stream->list
    (tail-degrees->graphs-stream
     (stream (graph-gen-data '() '(2 2 2 2 2 2) 0 '((0 1 2 3 4 5)) '((0 1 2 3 4 5))))))
   '(((0 1) (0 2) (1 3) (2 4) (3 5) (4 5)))))
                                                                
 

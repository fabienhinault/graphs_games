#lang racket

; stream version


(define (degrees->graphs-stream degrees)
  (define categories (get-degrees-categories degrees 0))
  (tail-degrees->graphs-stream 
   (stream
    (graph-gen-data '() degrees 0 categories categories))))


(define (get-edges-subgraphs-stream edges category-nbs degrees first-vertex new-all-categories
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
  (define sub-graphs-stream (rec-degrees->graphs-stream new-degrees
                                                    (+ 1 first-vertex)
                                                    new-all-categories
                                                    new-new-v1-categories
                                                    new-filled-slots))
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
(define (rec-degrees->graphs-stream degrees first-vertex all-categories first-vertex-categories filled-slots)
  (define length-degrees (length degrees))
  (cond ((equal? degrees '())
         empty-stream)
        ((equal? degrees '(0))
          (stream '())) ; the empty graph
        ((and (equal? 0 (car degrees)) (memf (λ (deg) (> deg 0)) degrees))
         empty-stream) ; break as leading to disconnected graphs
        ((null? (car all-categories))
         (rec-degrees->graphs-stream degrees first-vertex (cdr all-categories) first-vertex-categories filled-slots))
        ((null? (car first-vertex-categories))
         (rec-degrees->graphs-stream degrees first-vertex all-categories (cdr first-vertex-categories filled-slots)))

        ((memf (λ (deg) (>= deg length-degrees)) degrees)
         empty-stream)
        (else (let* ((first-second-same-category (member (+ 1 first-vertex) (car all-categories)))
                     ;remove current vertex from categories
                     (new-all-categories (filter-out-vertex-from-categories first-vertex all-categories))
                     (new-v1-categories (filter-out-vertex-from-categories first-vertex first-vertex-categories))
                     (neighbourss (parts new-v1-categories (car degrees) (car filled-slots)))
                     (category-nbss (nbss new-v1-categories (car degrees) (car filled-slots)))
                     (edgess (map (λ (ns) (neighbours->edges first-vertex ns)) neighbourss)))
                (stream-fold
                  (λ (acc edges)
                    (stream-append
                     acc
                     (get-edges-subgraphs-stream edges degrees first-vertex new-all-categories
                                                new-v1-categories first-second-same-category filled-slots)))
                  empty-stream
                  edgess)))))

(struct graph-gen-data (edges degrees first-vertex all-categories first-vertex-categories filled-slots))

(define (get-edges-gen-data-stream data-edges edges category-nbs degrees first-vertex new-all-categories
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
  (graph-gen-data (append data-edges edges) new-degrees (+ 1 first-vertex) new-all-categories
                  new-new-v1-categories new-filled-slots))

(define (tail-degrees->graphs-stream gen-datas)
  (if (stream-empty? gen-datas) empty-stream
      (let* ((data (stream-first gen-datas))
             (data-edges (graph-gen-data-edges data))
             (degrees (graph-gen-data-degrees data))
             (first-vertex (graph-gen-data-first-vertex data))
             (all-categories (graph-gen-data-all-categories data))
             (first-vertex-categories (graph-gen-data-first-vertex-categories data))
             (filled-slots (graph-gen-data-filled-slots data)))
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
                  (neighbourss (parts new-v1-categories (car degrees) (car filled-slots)))
                  (category-nbss (nbss new-v1-categories (car degrees) (car filled-slots)))
                  (edgess (map (λ (ns) (neighbours->edges first-vertex ns)) neighbourss)))
             (tail-degrees->graphs-stream
              (stream-append
               (stream-map
                (λ (edges)
                  (get-edges-gen-data-stream data-edges edges degrees first-vertex new-all-categories
                                             new-v1-categories first-second-same-category filled-slots))
                edgess)
               (stream-rest gen-datas)))))))))
                

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

  (check-equal? (get-new-degrees '((2 3)) '(1) 2) '(0))
  (check-equal? (rec-parts-w/nb-categories '(((2 3))) 1) '(((2 3))))
  (check-equal? (get-edge-categories 2 '((3))) '(((2 3))))
  (check-equal? (filter-out-vertex-from-categories 2 '((2 3))) '((3)))
  (check-equal? (rec-degrees->graphs '(1 1) 2 '((2 3)) '((2 3)) (set)) '(((2 3))))
  (check-equal? (stream->list (rec-degrees->graphs-stream  '(1 1) 2 '((2 3)) '((2 3))))
                (rec-degrees->graphs '(1 1) 2 '((2 3)) '((2 3)) (set)))

  (check-equal? (get-new-degrees '((1 2) (1 3)) '(2 2) 1) '(1 1))
  (check-equal? (rec-parts-w/nb-categories '(((1 2) (1 3))) 2) '(((1 2) (1 3))))
  (check-equal? (get-edge-categories 1 '((2 3))) '(((1 2) (1 3))))
  (check-equal? (filter-out-vertex-from-categories 1 '((1) (2 3))) '((2 3)))
  (check-equal? (rec-degrees->graphs '(2 2 2) 1 '((1) (2 3)) '((1) (2 3)) (set)) '(((1 2) (1 3) (2 3))))
  (check-equal? (stream->list (rec-degrees->graphs-stream  '(2 2 2) 1 '((1) (2 3)) '((1) (2 3))))
                (rec-degrees->graphs '(2 2 2) 1 '((1) (2 3)) '((1) (2 3)) (set)))

  (check-equal? (get-new-degrees '((0 2) (0 3)) '(2 3 3) 0) '(2 2 2))

  (check-equal? (rec-parts-w/nb-categories '() '(2)) '())
  (check-equal? (get-edge-categories 3 '()) '())
  (check-equal? (filter-out-vertex-from-categories 3 '((3))) '())
  (check-equal? (rec-degrees->graphs '(2) 3 '((3)) '((3)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream  '(2) 3 '((3)) '((3))))
                (rec-degrees->graphs '(2) 3 '((3)) '((3)) (set)))

  (check-equal? (get-new-degrees '((2 3)) '(3) 2) '(2))
  (check-equal? (rec-parts-w/nb-categories '(((2 3))) 1) '(((2 3))))
  (check-equal? (get-edge-categories 2 '((3))) '(((2 3))))
  (check-equal? (filter-out-vertex-from-categories 2 '((2) (3))) '((3)))
  (check-equal? (rec-degrees->graphs '(1 3) 2 '((2) (3)) '((2) (3)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream  '(1 3) 2 '((2) (3)) '((2) (3))))
                (rec-degrees->graphs '(1 3) 2 '((2) (3)) '((2) (3)) (set)))

  (check-equal? (get-new-degrees '((1 2)) '(2 3) 1) '(1 3))
  (check-equal? (rec-parts-w/nb-categories '(((1 2)) ((1 3))) 1) '(((1 2)) ((1 3))))
  (check-equal? (get-edge-categories 1 '((2) (3))) '(((1 2)) ((1 3))))
  (check-equal? (filter-out-vertex-from-categories 1 '((1) (2) (3))) '((2) (3)))
  (check-equal? (rec-degrees->graphs '(1 2 3) 1 '((1) (2) (3)) '((1) (2) (3)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream   '(1 2 3) 1 '((1) (2) (3)) '((1) (2) (3))))
                (rec-degrees->graphs  '(1 2 3) 1 '((1) (2) (3)) '((1) (2) (3)) (set)))

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
  (check-equal? (remove-categories-before-first-joined '((4)) '([3 4])) '((4)))
  (check-equal? (get-new-degrees '([3 4]) '(2) 3) '(1))
  (check-equal? (rec-parts-w/nb-categories '(([3 4])) 0) '(()))
  (check-equal? (get-edge-categories 3 '((4))) '(([3 4])))
  (check-equal? (filter-out-vertex-from-categories 3 '((4))) '((4)))
  (check-equal? (rec-degrees->graphs '(0 2) 3 '((4)) '((4)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream '(0 2) 3 '((4)) '((4))))
                (rec-degrees->graphs '(0 2) 3 '((4)) '((4)) (set)))
  (check-equal? (remove-categories-before-first-joined '((3) (4)) '([2 3] [2 4])) '((3) (4)))
  (check-equal? (get-new-degrees '([2 3] [2 4]) '(1 3) 2) '(0 2))
  (check-equal? (rec-parts-w/nb-categories  '(([2 3]) ([2 4])) 2) '(([2 3] [2 4])))
  (check-equal? (get-edge-categories 2 '((3) (4))) '(([2 3]) ([2 4])))
  (check-equal? (filter-out-vertex-from-categories 2 '((2) (3) (4))) '((3) (4)))
  (check-equal? (rec-degrees->graphs '(2 1 3) 2 '((2) (3) (4)) '((2) (3) (4)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream '(2 1 3) 2 '((2) (3) (4)) '((2) (3) (4))))
                (rec-degrees->graphs '(2 1 3) 2 '((2) (3) (4)) '((2) (3) (4)) (set)))
  (check-equal? (remove-categories-before-first-joined '((2) (3) (4)) '([1 3])) '((3) (4)))
  (check-equal? (get-new-degrees '([1 3]) '(2 2 3) 1) '(2 1 3))
              
  (check-equal? (rec-degrees->graphs '(1 2 3) 2 '((2) (3) (4)) '((2) (3) (4)) (set)) '())
  (check-equal? (stream->list (rec-degrees->graphs-stream '(1 2 3) 2 '((2) (3) (4)) '((2) (3) (4))))
                (rec-degrees->graphs '(1 2 3) 2 '((2) (3) (4)) '((2) (3) (4)) (set)))
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

  (check-equal? (get-new-degrees '([0 1] [0 3]) '(2 2 3 3) 0) '(1 2 2 3))
            
  (check-equal? (rec-degrees->graphs '(1 1 3 3) 1 '((1 2) (3 4)) '((1 2) (3 4)) (set)) '())
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
   '(((0 1) (0 2) (1 3) (2 4) (3 5) (4 5))))

;  (check-equal? (degreess 6 14 2 5) '((2 2 2 2 2 4) (2 2 2 2 3 3)))

  ; all graphs beginning with ((0 1) (0 5)...) should be removed as isomorphic to
  ; '((0 1) (0 2) (1 5) (2 5) (3 4) (3 5) (4 5))
  ; => no 2,2 after 2,4
  ; 0-2   3
  ; |  \ /|
  ; |   5 |
  ; |  / \|
  ; 1-'   4
  (check-equal? (degrees->graphs '(2 2 2 2 2 4)) '(((0 1) (0 2) (1 5) (2 5) (3 4) (3 5) (4 5))))
  )
; (2 2 2 2 3 3)
;  (check-equal? (degreess 6 16 2 5) '((2 2 2 2 3 5) (2 2 2 2 4 4) (2 2 2 3 3 4) (2 2 3 3 3 3)))
;  (check-equal? (degreess 6 18 2 5) '((2 2 2 2 5 5)
;                                      (2 2 2 3 4 5)
;                                      (2 2 2 4 4 4)
;                                      (2 2 3 3 3 5)
;                                      (2 2 3 3 4 4)
;                                      (2 3 3 3 3 4)
;                                      (3 3 3 3 3 3)))
;  (check-equal? (degreess 6 20 2 5) '((2 2 2 4 5 5)
;                                      (2 2 3 3 5 5)
;                                      (2 2 3 4 4 5)
;                                      (2 2 4 4 4 4)
;                                      (2 3 3 3 4 5)
;                                      (2 3 3 4 4 4)
;                                      (3 3 3 3 3 5)
;                                      (3 3 3 3 4 4)))
;  (check-equal? (degreess 6 22 2 5) '((2 2 3 5 5 5)
;                                      (2 2 4 4 5 5)
;                                      (2 3 3 4 5 5)
;                                      (2 3 4 4 4 5)
;                                      (2 4 4 4 4 4)
;                                      (3 3 3 3 5 5)
;                                      (3 3 3 4 4 5)
;                                      (3 3 4 4 4 4)))
;  (check-equal? (degreess 6 24 2 5)   '((2 2 5 5 5 5)
;                                        (2 3 4 5 5 5)
;                                        (2 4 4 4 5 5)
;                                        (3 3 3 5 5 5)
;                                        (3 3 4 4 5 5)
;                                        (3 4 4 4 4 5)
;                                        (4 4 4 4 4 4)))
;  (check-equal? (degreess 6 26 2 5) '((2 4 5 5 5 5) (3 3 5 5 5 5) (3 4 4 5 5 5) (4 4 4 4 5 5)))
;  (check-equal? (degreess 6 28 2 5) '((3 5 5 5 5 5) (4 4 5 5 5 5)))
;  (check-equal? (degreess 6 30 2 5) '((5 5 5 5 5 5))))



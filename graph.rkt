#lang racket

(require rackunit)
(require srfi/67) ; compare procedures
(require racket/random)
(require racket/trace)
(require json)
(require net/base64)

(define (rec-couples l)
    (if (null? l)
        '()
        (append
         (map
          (lambda (_) (list (car l) _))
          (cdr l))
         (rec-couples (cdr l)))))

(check-equal? (rec-couples '(a z e r)) '((a z) (a e) (a r) (z e) (z r) (e r)) "rec_couples")

(define (tailrec-couples res l)
    (if (null? l)
        res
        (tailrec-couples
         (append
          res
          (map (lambda (_) (list (car l) _))
               (cdr l)))
         (cdr l))))

(check-equal? (tailrec-couples '() '(a z e r))
              '((a z) (a e) (a r) (z e) (z r) (e r)) "tailrec-couples")
(check-equal? (tailrec-couples '() '(a e r z))
              '((a e) (a r) (a z) (e r) (e z) (r z)) "tailrec-couples")

(define (tailrec-sorted-couples res l less-than?)
    (if (null? l)
        res
        (tailrec-sorted-couples
         (append
          res
          (map (lambda (_) (sort (list (car l) _) less-than?))
               (cdr l)))
         (cdr l)
         less-than?)))

(check-equal? (tailrec-sorted-couples '() '(a z e r) symbol<?)
              '((a z) (a e) (a r) (e z) (r z) (e r)) "tailrec-sorted-couples")
(check-equal? (tailrec-sorted-couples '() '(a e r z) symbol<?)
              '((a e) (a r) (a z) (e r) (e z) (r z)) "tailrec-sorted-couples")

(define (tailrec res l update-res)
    (if (null? l)
        res
        (tailrec
         (update-res res l)
         (cdr l)
         update-res)))

(define (rec-parts l)
    (if (null? l)
        '(())
        (append
         (map
          (lambda (_) (cons (car l) _))
          (rec-parts (cdr l)))
         (rec-parts (cdr l)))))

(check-equal? (rec-parts '(a z e r)) '((a z e r)
                                       (a z e)
                                       (a z r)
                                       (a z)
                                       (a e r)
                                       (a e)
                                       (a r)
                                       (a)
                                       (z e r)
                                       (z e)
                                       (z r)
                                       (z)
                                       (e r)
                                       (e)
                                       (r)
                                       ()))

(define (tailrec-parts-update-result tmp-res x)
    (append
     tmp-res
     (map
      (lambda (_) (cons x _))
      tmp-res)))

(check-equal? (tailrec-parts-update-result '((e) ()) 'z) '((e) () (z e) (z)))

(define (tailrec-parts res l)
    (if (null? l)
        res
        (tailrec-parts
         (tailrec-parts-update-result res (car l))
         (cdr l))))

(check-equal? (tailrec-parts '(()) '(a z e r)) '(()
                                                 (a)
                                                 (z)
                                                 (z a)
                                                 (e)
                                                 (e a)
                                                 (e z)
                                                 (e z a)
                                                 (r)
                                                 (r a)
                                                 (r z)
                                                 (r z a)
                                                 (r e)
                                                 (r e a)
                                                 (r e z)
                                                 (r e z a)))

(define (graph-first-node g)
  (caar g))

; second node of first edge of g
(define (graph-second-node g)
  (cadar g))

(define (graphs1 l)
  (tailrec-parts '(()) (tailrec-couples '() l)))

(check-equal?
 (graphs1 '(a e z))
 '(() ((a e)) ((a z)) ((a z) (a e)) ((e z)) ((e z) (a e)) ((e z) (a z)) ((e z) (a z) (a e))))

(define (contains-deep? l x)
  (member x (flatten l)))

; does the graph g contains all of elements of the list l
(define (contains-all? g l)
  (cond ((null? l) true)
        ((null? g) l)
        (else (contains-all?
               (cdr g)
               (filter (λ(s) (not (or (equal? s (graph-first-node g))
                                      (equal? s (graph-second-node g)))))
                       l)))))

(define (add-absent-vertices g l)
  (let ((? (contains-all? g l)))
    (if (equal? ? #t)
        g
        (append g (map (λ (_) (list _ (random-ref l))) ?)))))
         

(define (tailrec-graph->node-set graph nodes-set)
  (if (null? graph)
      nodes-set
      (tailrec-graph->node-set
       (cdr graph)
       (set-add (set-add nodes-set (graph-first-node graph)) (graph-second-node graph)))))

(define (graph->node-set graph)
  (tailrec-graph->node-set graph (set)))

; nodes-so-far-set = vertices reached from the first one
; unused-edges = edges that do not connect to the vertices of nodes-so-far-set
(define (tailrec-is-graph-connected? graph all-nodes-set nodes-so-far-set unused-edges)
  (if (null? graph)
      (if (null? unused-edges)
          (set=? all-nodes-set nodes-so-far-set)
          (if (set-empty? (set-intersect (graph->node-set unused-edges) nodes-so-far-set))
              ; not connected. propose an edge to add.
              (list (graph-first-node unused-edges) (set-first nodes-so-far-set))
              (tailrec-is-graph-connected? unused-edges all-nodes-set nodes-so-far-set '())))
      (cond ((set-member? nodes-so-far-set (graph-first-node graph))
             ; if the first vertex of the remaining edges is in the nodes reached,
             ; then add the second vertex
             (tailrec-is-graph-connected?
              (cdr graph)
              all-nodes-set
              (set-add nodes-so-far-set (graph-second-node graph))
              unused-edges))
            ((set-member? nodes-so-far-set (graph-second-node graph))
             ; if not, but the second vertex is, then add the first one
             (tailrec-is-graph-connected?
              (cdr graph)
              all-nodes-set
              (set-add nodes-so-far-set (graph-first-node graph))
              unused-edges))
            (else
             ; if none are add the first edge to unused-edges
             (tailrec-is-graph-connected?
              (cdr graph)
              all-nodes-set
              nodes-so-far-set
              (cons (car graph) unused-edges))))))

(define set-abcd  (set 'a 'b 'c 'd))
(check-true (tailrec-is-graph-connected? '((a b) (c d) (a c))
                                        (set 'a 'b 'c 'd)
                                        (set 'a)
                                        '()))
(check-true (tailrec-is-graph-connected? '((a b) (a d) (a c))
                                        (set 'a 'b 'c 'd)
                                        (set 'a)
                                        '()))
(check-not-equal? true (tailrec-is-graph-connected? '((a b) (c d)) (set 'a 'b 'c 'd) (set 'a) '()))
(check-not-equal? true (tailrec-is-graph-connected? '((c d)) (set 'a 'b 'c 'd) (set 'a ' b) '()))
(check-not-equal? true (tailrec-is-graph-connected? '() (set 'a 'b 'c 'd) (set 'a ' b) '((c d))))
(check-true (tailrec-is-graph-connected? '((a b) (b c)) (set 'a 'b 'c) (set 'a) '()))
(check-true (tailrec-is-graph-connected? '((a b) (a c)) (set 'a 'b 'c) (set 'a) '()))
(check-true (tailrec-is-graph-connected? '((a b)) (set 'a 'b) (set 'a) '()))
(check-true (tailrec-is-graph-connected? '() (set) (set) '()))

(define (rec-connect-graph g nodes-set)
  (let ((complete (tailrec-is-graph-connected? g nodes-set (set (graph-first-node g)) '())))
    (cond ((equal? complete #t)
           g)
          ((equal? complete #f)
           (raise g))
          (else 
           (rec-connect-graph (cons complete g) nodes-set)))))

(if (equal? (version) "8.9")
    (check-equal? (rec-connect-graph '((a b) (c d)) set-abcd) '((c b) (a b) (c d)))
    (check-equal? (rec-connect-graph '((a b) (c d)) set-abcd) '((c a) (a b) (c d))))

(define (graphs1_1 l)
  (filter (λ(g) (equal? #t (contains-all? g l))) (graphs1 l)))

(check-equal?
 (graphs1_1 '(a e z))
 '(((a z) (a e)) ((e z) (a e)) ((e z) (a z)) ((e z) (a z) (a e))))

; util function for tailrec-sorted-parts
(define (tailrec-sorted-parts-update-result tmp-res x less-than?)
  (append
   tmp-res
   (map
    (lambda (_) (sort (cons x _) less-than?))
    tmp-res)))

(check-equal? (tailrec-sorted-parts-update-result '((e) ()) 'z symbol<?) '((e) () (e z) (z)))

(define (tailrec-sorted-parts res l less-than?)
  (if (null? l)
      res
      (tailrec-sorted-parts
       (tailrec-sorted-parts-update-result res (car l) less-than?)
       (cdr l)
       less-than?)))

(check-equal? (tailrec-sorted-parts '(()) '(a z e) symbol<?)
              '(() (a) (z) (a z) (e) (a e) (e z) (a e z)))

(define (deep f l)
    (cond ((null? l) '())
          ((pair? l) (cons (deep f (car l)) (deep f (cdr l))))
          (else (f l))))

(define (replace-all-deep old new l)
  (cond ((null? l) '())
        ((equal? l old) new)
        ((pair? l) (cons (replace-all-deep old new (car l)) (replace-all-deep old new (cdr l))))
        (else l)))

(check-equal? (replace-all-deep 'a 'b '()) '())
(check-equal? (replace-all-deep 'a 'b 'a) 'b)
(check-equal? (replace-all-deep 'a 'b 'b) 'b)
(check-equal? (replace-all-deep 'a 'b 'z) 'z)
(check-equal? (replace-all-deep 'a 'b '((a z) (e r))) '((b z) (e r)))

; https://github.com/arclanguage/anarki/blob/master/arc.arc#L1844
(define (memo f)
  (let ((cache (make-hash)))
    (λ args
      (hash-ref cache args
                (λ ()
                  (let ((result (apply f args)))
                    (hash-set! cache args result)
                    result))))))

(define get-degree (memo (λ (symbol graph)
  (length (filter (λ (edge) (member symbol edge)) graph)))))

(check-equal? (get-degree 'a '((a z) (a e))) 2)
(check-equal? (get-degree 'z '((a z) (a e))) 1)
(check-equal? (get-degree 'e '((a z) (a e))) 1)

(define (new-name-numeric-generator)
  (let ((count 0))
    (λ () (begin0
            count
            (set! count (+ 1 count))))))

; auxiliary function for get-graph-nodes-by-degrees
(define (add-node-by-degree acc v degree)
  (let ((all-nodes (vector-ref acc 0))
        (deg-nodes (vector-ref acc degree)))
    (when (not (member v all-nodes))
      (let ((new-all (cons v all-nodes))
            (new-deg (cons v deg-nodes)))
        (vector-set! acc 0 new-all)
        (vector-set! acc degree new-deg)))
    acc))

(check-equal? (add-node-by-degree (make-vector 6 '()) 'a 3)
              '#((a) () () (a) () ()))

; auxiliary function for get-graph-nodes-by-degrees
(define (add-edge-nodes-by-degrees* graph)
  (λ (edge acc)
    (let* ((v1 (car edge))
           (deg1 (get-degree v1 graph))
           (v2 (cadr edge))
           (deg2 (get-degree v2 graph)))
      (add-node-by-degree (add-node-by-degree acc v1 deg1) v2 deg2))))

(check-equal? ((add-edge-nodes-by-degrees* '((a b))) '(a b) (make-vector 2 '()))
              '#((b a) (b a)))

(let ((add-edge-nodes-by-degrees (add-edge-nodes-by-degrees* '((a b) (a c))))
      (acc (make-vector 3 '())))
  (check-equal? (add-edge-nodes-by-degrees '(a b) acc)
                '#((b a) (b) (a)))
  (check-equal? acc '#((b a) (b) (a)))
  (check-equal? (add-edge-nodes-by-degrees '(a c) acc)
                '#((c b a) (c b) (a)))
  )

(define (get-graph-nodes-by-degrees graph nb-vertices)
  (vector-map reverse
              (foldl (add-edge-nodes-by-degrees* graph)
                     (make-vector nb-vertices '())
                     graph)))

(check-equal? (get-graph-nodes-by-degrees '((a b) (a c)) 3)
              '#((a b c) (b c) (a)))


(define (add-edge-to-graph-vector edge acc-vector-nextss)
  (vector-set! acc-vector-nextss (car edge)
               (cons (cadr edge) (vector-ref acc-vector-nextss (car edge))))
  (vector-set! acc-vector-nextss (cadr edge)
               (cons (car edge) (vector-ref acc-vector-nextss (cadr edge))))
  acc-vector-nextss)

(define (get-graph-nextss graph nb-vertices)
  (deep ~a (vector->list (foldl add-edge-to-graph-vector (make-vector nb-vertices '()) graph))))

(check-equal? (get-graph-nextss '((0 2) (1 2)) 3) '(("2") ("2") ("1" "0")))

         
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

(check-equal? (hash->list (get-degree-renaming '((0 1) (0 2)) 3 (new-name-numeric-generator)))
              '((0 . 2) (2 . 1) (1 . 0)))

(define (get-graph-degrees graph)
  (deep (λ (_) (get-degree _ graph)) graph))

(check-equal? (get-graph-degrees '((a z) (a e))) '((2 1) (2 1)))

(define (vertice<?* graph)
  (λ (v1 v2)
    (> 0
       (refine-compare
        (- (get-degree v1 graph) (get-degree v2 graph))
        (symbol-compare v1 v2)))))

(define (vertice-compare* graph)
  (λ (v1 v2)
    (refine-compare
     (integer-compare (get-degree v1 graph) (get-degree v2 graph))
     (symbol-compare v1 v2))))
  

(let* ((G '((a z) (a e)))
       (v<? (vertice<?* G)))
  (check-true (v<? 'e 'a))
  (check-true (v<? 'z 'a))
  (check-false (v<? 'a 'z))
  (check-false (v<? 'a 'e))
  (check-true (v<? 'e 'z))
  (check-false (v<? 'a 'e)))

(define (edge-degree-compare* get-vertex-degree)
  (λ (e1 e2)
    (refine-compare
        (integer-compare (get-vertex-degree (car e1)) (get-vertex-degree (car e2)))
        (integer-compare (get-vertex-degree (cadr e1)) (get-vertex-degree (cadr e2))))))

(define (edge-vertex-compare e1 e2)
  (refine-compare
   (integer-compare (car e1) (car e2))
   (integer-compare (cadr e1) (cadr e2))))

; the vertices are supposed to be sorted in the edge
(define (edge<?* get-vertex-degree)
  (let ((edge-degree-compare (edge-degree-compare* get-vertex-degree)))
    (λ (e1 e2)
      (< (refine-compare
          (edge-degree-compare e1 e2)
          (edge-vertex-compare e1 e2))
         0))))

(let* ((degrees (make-hash))
       (get-v-degree (λ (_) (hash-ref degrees _)))
       (vertice-compare (λ (v1 v2)
                          (refine-compare
                           (integer-compare (hash-ref degrees v1) (hash-ref degrees v2))
                           (integer-compare v1 v2))))
       (vertice<? (λ (v1 v2) (< (vertice-compare v1 v2)
                                0)))
       (graph '((0 1) (2 3) (1 3) (0 4) (1 4) (2 4))))
  (deep (λ (_) (hash-set! degrees _ (get-degree _ graph))) graph)
  (check-equal? (map (λ (_) (sort _ vertice<?)) graph)
                '((0 1) (2 3) (3 1) (0 4) (1 4) (2 4)))
  (check-equal? (get-v-degree '2) 2)
  (check-true ((edge<?* get-v-degree) '(2 3) '(0 4))))

(let* ((G '((0 2) (0 1)))
       (e<? (edge<?* (λ (_) (get-degree _ G)))))
  (check-false (e<? '(2 0) '(1 0)))
  (check-false (e<? '(2 0) '(1 0)))
  (check-true  (e<? '(1 0) '(2 0))))
  
(let* ((G '((0 1) (0 2) (1 4) (3 2) (3 4) (2 4)))
       ; degrees:  0:2  1:2  2:3  3:2  4:3 
       (e<? (edge<?* (λ (_) (get-degree _ G)))))
  (check-true  (e<? '(0 1) '(0 2)))
  (check-true  (e<? '(0 2) '(1 4)))
  (check-true  (e<? '(1 4) '(3 2)))
  (check-true  (e<? '(3 2) '(3 4)))
  (check-true  (e<? '(3 4) '(2 4))))
  

(define (edge-compare* graph)
  (let ((get-vertex-degree (λ (_) (get-degree _ graph))))
    (λ (e1 e2)
      (refine-compare
          (integer-compare (get-vertex-degree (car e1)) (get-vertex-degree (car e2))
          (integer-compare (get-vertex-degree (cadr e1)) (get-vertex-degree (cadr e2))
          (symbol-compare (car e1) (car e2))
          (symbol-compare (cadr e1) (cadr e2))))))))

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

(define (vertex-dot v)
    (~a v " [id=\"id" v "\"]" #\newline))

(define (edge-dot e)
  (~a (car e) " -- " (cadr e) " [class=\"_" (car e) " _" (cadr e)  "\"]" #\newline))

(check-equal?
 (edge-dot '(a z)) "a -- z [class=\"_a _z\"]
")

(define (graph-dot g vertices)
  (string-append*
   "strict graph {
node [shape=circle style=filled fillcolor=gray99]
"
   (append (map vertex-dot vertices)
           (map edge-dot g)
           (list "}
"))))

(define (graph-name g)
  (string-replace
   (string-replace
    (string-replace
     (bytes->string/utf-8 (base64-encode (list->bytes (flatten g)) ""))
     "+" "-")
    "/" "_")
   "=" ""))

(check-equal? (graph-name '((0 1) (0 2))) "AAEAAg")

(define (make-directory-and-parents dir)
  (when (not (directory-exists? dir))
    (make-directory-and-parents (simplify-path (build-path dir 'up)))
    (make-directory dir)))

(define (write-dot-file g dir vertices)
  (when (not (directory-exists? dir))
      (make-directory-and-parents dir))
  (with-output-to-file (~a dir "/" (graph-name g) ".dot")
    (λ() (printf (graph-dot g vertices))))
  (with-output-to-file (~a dir "/" (graph-name g) ".json")
    (λ() (write-json (get-graph-nextss g (length vertices))))))

(define (random-edge l)
  (let* ((v1 (random-ref l)))
    (list v1 (random-ref (remove v1 l)))))

(define (random-graph l nb-edges)
  (remove-duplicates (build-list nb-edges (λ (_) (random-edge l)))))

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

(define (get-graph-degree-1-vertices graph vertices)
  (filter (λ (_) (equal? 1 (get-degree _ graph)))
          vertices))

(define (make-all-vertices-degree2 graph vertices)
  (append graph
          (map (λ (_) (list _ (random-ref (remove _ vertices))))
               (get-graph-degree-1-vertices graph vertices))))

(define (create-random-edinburgh-graph vertices nb-edges)
  (rewrite-graph
   (rec-rename-graph-vertices
    (rewrite-graph 
     (rec-connect-graph
      (make-all-vertices-degree2
       (add-absent-vertices (random-graph vertices nb-edges) vertices)
       vertices)
      (list->set vertices)))
    (make-hash) (new-name-numeric-generator))))

(define (write-random-edinburgh-dot nb-vertices nb-edges)
  (let* ((vertices (range nb-vertices))
         (graph (create-random-edinburgh-graph vertices nb-edges))
         (final-nb-edges (length graph))
         (path (~a nb-vertices "/" final-nb-edges)))
    (write-dot-file graph path vertices)
    path))

;(for-each
;   (lambda (_) (write-dot-file _ 6))
;   (filter has-no-vertex-degree-1 _6))




#lang racket

(require rackunit)
(require srfi/67) ; compare procedures
(require racket/random)

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

(define (contains-all? g l)
  (cond ((null? l) true)
        ((null? g) false)
        (else (contains-all?
               (cdr g)
               (filter (λ(s) (not (or (equal? s (graph-first-node g))
                                      (equal? s (graph-second-node g))))) l)))))

(define (tailrec-graph->node-set graph nodes-set)
  (if (null? graph)
      nodes-set
      (tailrec-graph->node-set
       (cdr graph)
       (set-add (set-add nodes-set (graph-first-node graph)) (graph-second-node graph)))))

(define (graph->node-set graph)
  (tailrec-graph->node-set graph (set)))

(define (tailrec-is-graph-complete? graph all-nodes-set nodes-so-far-set unused-edges)
  (if (null? graph)
      (if (null? unused-edges)
          (set=? all-nodes-set nodes-so-far-set)
          (if (set-empty? (set-intersect (graph->node-set unused-edges) nodes-so-far-set))
              (list (graph-first-node unused-edges) (set-first nodes-so-far-set))
              (tailrec-is-graph-complete? unused-edges all-nodes-set nodes-so-far-set '())))
      (cond ((set-member? nodes-so-far-set (graph-first-node graph))
             (tailrec-is-graph-complete?
              (cdr graph)
              all-nodes-set
              (set-add nodes-so-far-set (graph-second-node graph))
              unused-edges))
            ((set-member? nodes-so-far-set (graph-second-node graph))
             (tailrec-is-graph-complete?
              (cdr graph)
              all-nodes-set
              (set-add nodes-so-far-set (graph-first-node graph))
              unused-edges))
            (else
             (tailrec-is-graph-complete?
              (cdr graph)
              all-nodes-set
              nodes-so-far-set
              (cons (car graph) unused-edges))))))

(define set-abcd  (set 'a 'b 'c 'd))
(check-true (tailrec-is-graph-complete? '((a b) (c d) (a c))
                                        (set 'a 'b 'c 'd)
                                        (set 'a)
                                        '()))
(check-true (tailrec-is-graph-complete? '((a b) (a d) (a c))
                                        (set 'a 'b 'c 'd)
                                        (set 'a)
                                        '()))
(check-equal? '(c b) (tailrec-is-graph-complete? '((a b) (c d)) (set 'a 'b 'c 'd) (set 'a) '()))
(check-equal? '(c b) (tailrec-is-graph-complete? '((c d)) (set 'a 'b 'c 'd) (set 'a ' b) '()))
(check-equal? '(c b) (tailrec-is-graph-complete? '() (set 'a 'b 'c 'd) (set 'a ' b) '((c d))))
(check-true (tailrec-is-graph-complete? '((a b) (b c)) (set 'a 'b 'c) (set 'a) '()))
(check-true (tailrec-is-graph-complete? '((a b) (a c)) (set 'a 'b 'c) (set 'a) '()))
(check-true (tailrec-is-graph-complete? '((a b)) (set 'a 'b) (set 'a) '()))
(check-true (tailrec-is-graph-complete? '() (set) (set) '()))

(define (rec-complete-graph g nodes-set)
  (let ((complete (tailrec-is-graph-complete? g nodes-set (set (graph-first-node g)) '())))
    (if (equal? complete #true)
        g
        (rec-complete-graph (cons complete g) nodes-set))))

(check-equal? (rec-complete-graph '((a b) (c d)) set-abcd) '((c b) (a b) (c d)))

(define (graphs1_1 l)
  (filter (λ(g) (contains-all? g l)) (graphs1 l)))

(check-equal?
 (graphs1_1 '(a e z))
 '(((a z) (a e)) ((e z) (a e)) ((e z) (a z)) ((e z) (a z) (a e))))

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

(check-equal? (tailrec-sorted-parts '(()) '(a z e) symbol<?) '(() (a) (z) (a z) (e) (a e) (e z) (a e z)))

(define (edge<? edge1 edge2)
  (let ((car1 (car edge1))
        (car2 (car edge2)))
    (or (symbol<? car1 car2)
        (and (not (symbol<? car2 car1)) (symbol<? (cadr edge1) (cadr edge2))))))

(check-true (edge<? '(a b) '(c d)))
(check-true (edge<? '(a b) '(a c)))
(check-false (edge<? '(a b) '(a b)))
(check-false (edge<? '(a c) '(a b)))
(check-false (edge<? '(c d) '(a b)))

(define (graph<? graph1 graph2)
  (or (< (length graph1) (length graph2))
      (and (equal? (length graph1) (length graph2))
           (not (null? graph1))
           (let ((edge1 (car graph1))
                 (edge2 (car graph2)))
             (or (edge<? edge1 edge2)
                 (and (not (edge<? edge2 edge1)) (graph<? (cdr graph1) (cdr graph2))))))))

(check-false (graph<? '() '()))
(check-true  (graph<? '() '((a b))))
(check-false (graph<? '((a b)) '() ))
(check-true  (graph<? '((a b)) '((a b) (c d))))
(check-false (graph<? '((a b) (c d)) '((a b))))
(check-true  (graph<? '((a b)) '((a c))))
(check-false (graph<? '((a c)) '((a b))))
(check-false (graph<? '((a b)) '((a b))))
(check-false (graph<? '((a b) (a c)) '((a b) (a c))))
(check-false (graph<? '((a b) (a d)) '((a b) (a c))))
(check-true  (graph<? '((a b) (a c)) '((a b) (a d))))

(define (graphs2 l)
  (sort (tailrec-sorted-parts '(()) (tailrec-sorted-couples '() l symbol<?) edge<?) graph<?))

(check-equal?
 (graphs2 '(a e z))
'(() ((a e)) ((a z)) ((e z)) ((a e) (a z)) ((a e) (e z)) ((a z) (e z)) ((a e) (a z) (e z))))

(define (replace-all-deep old new l)
    (cond ((null? l)
        '())
          ((equal? l old) new)
        ((pair? l) (cons (replace-all-deep old new (car l)) (replace-all-deep old new (cdr l))))
        (else l)))

(check-equal? (replace-all-deep 'a 'b '()) '())
(check-equal? (replace-all-deep 'a 'b 'a) 'b)
(check-equal? (replace-all-deep 'a 'b 'b) 'b)
(check-equal? (replace-all-deep 'a 'b 'z) 'z)
(check-equal? (replace-all-deep 'a 'b '((a z) (e r))) '((b z) (e r)))

(define (topo-graphs2 l)
    (let ((syms (map (λ (_) (gensym)) (range (length l)))))
      (map
       (λ (graph)
         (let ((uniqs (remove-duplicates (flatten graph))))
           (foldl replace-all-deep graph uniqs (take l (length uniqs)))))
       (graphs2 syms))))

(check-equal? (topo-graphs2 '(a e z))
              '(() ((a e)) ((a e)) ((a e)) ((a e) (a z)) ((a e) (e z)) ((a e) (z e)) ((a e) (a z) (e z))))

;'(() ((a e)) ((a z)) ((e z)) ((a e) (a z)) ((a e) (e z)) ((a z) (e z)) ((a e) (a z) (e z))))

(define (check-topo-graph2 l graph expected-uniqs expected-tgraph)
  (let* ((uniqs (remove-duplicates (flatten graph)))
         (tgraph (foldl replace-all-deep graph uniqs (take l (length uniqs)))))
    (check-equal? uniqs expected-uniqs)
    (check-equal? tgraph expected-tgraph)))

(check-topo-graph2 '(j k l) '((a e)) '(a e) '((j k)))
(check-topo-graph2 '(j k l) '((a z)) '(a z) '((j k)))
(check-topo-graph2 '(j k l) '((e z)) '(e z) '((j k)))
(check-topo-graph2 '(j k l) '((a e) (a z)) '(a e z) '((j k) (j l)))
(check-topo-graph2 '(j k l) '((a e) (e z)) '(a e z) '((j k) (k l)))
(check-topo-graph2 '(j k l) '((a z) (e z)) '(a z e) '((j k) (l k)))
(check-topo-graph2 '(j k l) '((a e) (a z) (e z)) '(a e z) '((j k) (j l) (k l)))


(define (get-degree symbol graph)
  (length (filter (λ (edge) (member symbol edge)) graph)))

(check-equal? (get-degree 'a '((a z) (a e))) 2)
(check-equal? (get-degree 'z '((a z) (a e))) 1)
(check-equal? (get-degree 'e '((a z) (a e))) 1)

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

; the vertices are supposed to be sorted in the edge
(define (edge<?* graph)
  (let ((vertice-compare (vertice-compare* graph)))
    (λ (e1 e2)
      (> 0
         (refine-compare
          (vertice-compare (car e1) (car e2))
          (vertice-compare (cadr e1) (cadr e2)))))))


(let* ((G '((a z) (a e)))
       (e<? (edge<?* G)))
  (check-false (e<? '(z a) '(e a)))
  (check-false (e<? '(z a) '(e a)))
  (check-true  (e<? '(e a) '(z a))))
  

(define (edge-compare graph)
  (let ((vertice-compare (vertice-compare* graph)))
    (λ (e1 e2)
      (refine-compare
       (vertice-compare (car e1) (car e2))
       (vertice-compare (cadr e1) (cadr e2))))))

(define (graphs3 l)
  (sort (tailrec-sorted-parts '(()) (tailrec-sorted-couples '() l symbol<?) edge<?) graph<?))

(define (edge-dot e)
  (~a (car e) " -- " (cadr e) #\newline))

(check-equal?
 (edge-dot '(a z)) "a -- z
")

(define (graph-dot g)
  (string-append*
   "strict graph {
"
   (append (map edge-dot g)
                (list "}
"))))

(define (graph-name g)
  (~a (apply string-append (map symbol->string (flatten g))) ".dot"))

(define (write-dot-file g)
  (with-output-to-file (graph-name g)
    (λ() (printf (graph-dot g)))))

(define (random-edge l)
  (let* ((v1 (random-ref l)))
    (list v1 (random-ref (remove v1 l)))))

(define (random-graph l nb-edges)
  (build-list nb-edges (λ(_) (random-edge l))))

(define (symbols n)
   (map (λ(_) (string->symbol
               (string
                (integer->char
                 (+ _ (char->integer #\a))))))
        (range n)))

(check-equal? (symbols 3) '(a b c))


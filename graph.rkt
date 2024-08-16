#lang racket

(require rackunit)
(require srfi/67) ; compare procedures
(require racket/random)
(require racket/trace)
(require json)
(require net/base64)
(require file/md5)

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

; partitions of l having nb elements
(define (rec-parts-w/nb l nb)
    (cond ((or (< (length l) nb) (< nb 0)) '())
          ((equal? (length l) nb)
           (list l))
          (else
           ; the parts of l of cardinal nb are the ones containing (car l)
           ; union the ones not containing (car l)
           (append
                 (map (λ (_) (cons (car l) _))
                      (rec-parts-w/nb (cdr l) (- nb 1)))
                 (rec-parts-w/nb (cdr l) nb)))))

(check-equal? (rec-parts-w/nb '(0) 0) '(()))
(check-equal? (rec-parts-w/nb '(0) 1) '((0)))
(check-equal? (rec-parts-w/nb '(0) 2) '())
(check-equal? (rec-parts-w/nb '(0 1) 1) '((0) (1)))
(check-equal? (rec-parts-w/nb '(0 1) 2) '((0 1)))
(check-equal? (rec-parts-w/nb '(0 1 2) 1) '((0) (1) (2)))
(check-equal? (rec-parts-w/nb '(0 1 2) 2) '((0 1) (0 2) (1 2)))
(check-equal? (rec-parts-w/nb '(0 1 2) 3) '((0 1 2)))
(check-equal? (rec-parts-w/nb '(0 1 2 3) 1) '((0) (1) (2) (3)))
(check-equal? (rec-parts-w/nb '(0 1 2 3) 2) '((0 1) (0 2) (0 3) (1 2) (1 3) (2 3)))
(check-equal? (rec-parts-w/nb '(0 1 2 3) 3) '((0 1 2) (0 1 3) (0 2 3) (1 2 3)))
(check-equal? (rec-parts-w/nb '(0 1 2 3) 4) '((0 1 2 3)))


; partitions of (apply append categories) having nb elements,
; always taking the first elements in each category
(define (rec-parts-w/nb-categories categories nb)
    (cond ((null? categories)
           '())
          ((equal? 0 nb)
           '(()))
          ((null? (car categories))
           (rec-parts-w/nb-categories (cdr categories) nb))
          (else (append
                 (map (λ (_) (cons (caar categories) _))
                      (rec-parts-w/nb-categories (cons (cdar categories) (cdr categories)) (- nb 1)))
                 (rec-parts-w/nb-categories (cdr categories) nb)))))

(check-equal? (rec-parts-w/nb-categories '((0)) 0) '(()))
(check-equal? (rec-parts-w/nb-categories '(()) 0) '(()))
(check-equal? (rec-parts-w/nb-categories '() 1) '())
(check-equal? (rec-parts-w/nb-categories '((0)) 1) '((0)))
(check-equal? (rec-parts-w/nb-categories '((0)) 2) '())
(check-equal? (rec-parts-w/nb-categories '((0 1)) 1) '((0)))
(check-equal? (rec-parts-w/nb-categories '(() (1)) 0) '(()))
(check-equal? (rec-parts-w/nb-categories '((0) (1)) 1) '((0) (1)))
(check-equal? (rec-parts-w/nb-categories '((0 1)) 2) '((0 1)))
(check-equal? (rec-parts-w/nb-categories '((0) (1)) 2) '((0 1)))
(check-equal? (rec-parts-w/nb-categories '((0 1 2)) 1) '((0)))
(check-equal? (rec-parts-w/nb-categories '((0) (1 2)) 1) '((0) (1)))
(check-equal? (rec-parts-w/nb-categories '((0) (1 2)) 2) '((0 1) (1 2)))
(check-equal? (rec-parts-w/nb-categories '((0 1) (2)) 1) '((0) (2)))
(check-equal? (rec-parts-w/nb-categories '((0) (1) (2)) 1) '((0) (1) (2)))
(check-equal? (rec-parts-w/nb-categories '((0 1 2)) 2) '((0 1)))
(check-equal? (rec-parts-w/nb-categories '((0) (1) (2)) 2) '((0 1) (0 2) (1 2)))
(check-equal? (rec-parts-w/nb-categories '((0 1 2)) 3) '((0 1 2)))
(check-equal? (rec-parts-w/nb-categories '((0 1 2 3)) 1) '((0)))
(check-equal? (rec-parts-w/nb-categories '((0) (1 2 3)) 1) '((0) (1)))
(check-equal? (rec-parts-w/nb-categories '((0 1) (2 3)) 1) '((0) (2)))
(check-equal? (rec-parts-w/nb-categories '((0 1 2) (3)) 1) '((0) (3)))
(check-equal? (rec-parts-w/nb-categories '((0 1 2 3)) 2) '((0 1)))
(check-equal? (rec-parts-w/nb-categories '((0) (1 2 3)) 2) '((0 1) (1 2)))
(check-equal? (rec-parts-w/nb-categories '((0 1) (2 3)) 2) '((0 1) (0 2) (2 3)))
(check-equal? (rec-parts-w/nb-categories '((0 1 2) (3)) 2) '((0 1) (0 3)))
(check-equal? (rec-parts-w/nb-categories '((0 1 2 3)) 3) '((0 1 2)))
(check-equal? (rec-parts-w/nb-categories '((0) (1 2 3)) 3) '((0 1 2) (1 2 3)))
(check-equal? (rec-parts-w/nb-categories '((0 1) (2 3)) 3) '((0 1 2) (0 2 3)))
(check-equal? (rec-parts-w/nb-categories '((0 1 2 3)) 4) '((0 1 2 3)))
(check-equal? (rec-parts-w/nb-categories '((0 1) (2 3)) 4) '((0 1 2 3)))
(check-equal? (rec-parts-w/nb-categories '(([0 1]) ([0 2] [0 3])) 2) '(([0 1] [0 2]) ([0 2] [0 3])))
(check-equal? (rec-parts-w/nb-categories '(([1 2]) ([1 3])) 1) '(([1 2]) ([1 3])))


; partitions of (apply append categories) having nb elements,
; always taking the first elements in each category
(define (rec-parts-w/nb-categories-stream categories nb)
    (cond ((null? categories)
           empty-stream)
          ((equal? 0 nb)
           (stream '()))
          ((null? (car categories))
           (rec-parts-w/nb-categories-stream (cdr categories) nb))
          (else (stream-append
                 (stream-map (λ (_) (cons (caar categories) _))
                      (rec-parts-w/nb-categories-stream (cons (cdar categories) (cdr categories)) (- nb 1)))
                 (rec-parts-w/nb-categories-stream (cdr categories) nb)))))


(define (tailrec-parts-update-result tmp-res x)
    (append
     tmp-res
     (map
      (λ (_) (cons x _))
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

(define (take-if-more l nb)
  (let ((len (length l)))
    (if (<= len nb)
        l
        (take l nb))))

; tmp-res is a list of lists of partial lists
(define (tailrec-parts-w/nb-update-result lll x nb+1)
  (map append
       lll
       (take-if-more (cons '() (map (λ (ll) (map (λ (l) (cons x l)) ll))
                                    lll))
                     nb+1)))

(check-equal? (tailrec-parts-w/nb-update-result '((())) 0 1) '((())) )
(check-equal? (tailrec-parts-w/nb-update-result '((()) ()) 0 2) '((()) ((0))) )
 
(define (tailrec-parts-w/nb res l nb+1)
    (if (null? l)
        (last res)
        (tailrec-parts-w/nb
         (tailrec-parts-w/nb-update-result res (car l) nb+1)
         (cdr l)
         nb+1)))

(check-equal? (tailrec-parts-w/nb '((()) ()) '(0) 2) '((0)))

(define (parts-w/nb l nb)
  (tailrec-parts-w/nb (cons '(()) (make-list nb '())) l (+ nb 1)))

(check-equal? (parts-w/nb '(0) 0) '(()))
(check-equal? (parts-w/nb '(0) 1) '((0)))
(check-equal? (parts-w/nb '(0) 2) '())
(check-equal? (parts-w/nb '(0 1) 1) '((0) (1)))
(check-equal? (parts-w/nb '(0 1) 2) '((1 0)))
(check-equal? (parts-w/nb '(0 1 2) 1) '((0) (1) (2)))
(check-equal? (parts-w/nb '(0 1 2) 2) '((1 0) (2 0) (2 1)))
(check-equal? (parts-w/nb '(0 1 2) 3) '((2 1 0)))
(check-equal? (parts-w/nb '(0 1 2 3) 1) '((0) (1) (2) (3)))
(check-equal? (parts-w/nb '(0 1 2 3) 2) '((1 0) (2 0) (2 1) (3 0) (3 1) (3 2)))
(check-equal? (parts-w/nb '(0 1 2 3) 3) '((2 1 0) (3 1 0) (3 2 0) (3 2 1)))
(check-equal? (parts-w/nb '(0 1 2 3) 4) '((3 2 1 0)))

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

(define (graphs11 nb-vertices nb-edges)
  (parts-w/nb (tailrec-couples '() (range nb-vertices)) nb-edges))

;(remove-duplicates (map rewrite-graph (filter has-no-vertex-degree-1 (filter (λ (_) (equal? #t (contains-all? _ (range 9)))) (graphs11 9 13))))))
      

(check-equal? (graphs11 3 2) '(((0 2) (0 1)) ((1 2) (0 1)) ((1 2) (0 2))))

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
  (vector->list (foldl add-edge-to-graph-vector (make-vector nb-vertices '()) graph)))

(check-equal? (get-graph-nextss '((0 2) (1 2)) 3) '((2) (2) (1 0)))

         
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

(check-not-false (member (hash->list (get-degree-renaming '((0 1) (0 2)) 3 (new-name-numeric-generator)))
              '(((0 . 2) (2 . 1) (1 . 0)) ((0 . 2) (1 . 0) (2 . 1)))))

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

(define (get-graph-degree-1-vertices graph vertices)
  (filter (λ (_) (equal? 1 (get-degree _ graph)))
          vertices))

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

(check-equal?
 '()
 (get-graph-degree-1-vertices
  (rewrite-graph
   '((29 7) (17 9) (5 6) (4 15) (5 10) (19 29) (20 8) (17 5) (2 22) (24 18) (25 4) (17 13) (2 26)
            (9 10) (27 24) (23 10) (15 29) (12 6) (22 9) (15 18) (22 11) (2 21) (20 11) (29 12)
            (21 7) (2 8) (5 18) (15 20) (27 4) (7 2) (1 17) (16 8) (20 21) (8 22) (28 26) (18 17)
            (25 28) (2 18) (20 1) (5 20) (2 20) (3 23) (26 15) (24 19) (4 2) (18 15) (6 10) (21 22)
            (12 22) (0 21) (14 24) (0 26) (3 0) (13 14) (14 21) (16 29)))
  (range 30)))

(define (vertex-dot v)
    (~a v " [id=\"id" v "\"]" #\newline))

(define (edge-dot e)
  (~a (car e) " -- " (cadr e) " [class=\"_" (car e) "_ _" (cadr e)  "_\"]" #\newline))

(check-equal?
 (edge-dot '(a z)) "a -- z [class=\"_a_ _z_\"]
")

(define (graph-dot g vertices)
  (string-append*
   "strict graph {
node [shape=circle style=filled fillcolor=gray99 width=0.5 fixedsize=shape]
"
   (append (list "🤖 [id=\"robot_begins\" fontsize=23]\n")
           (map vertex-dot vertices)
           (map edge-dot g)
           (list "}
"))))

(define A0 (char->integer #\A))
(define a26 (- (char->integer #\a) 26))
(define _052 (- (char->integer #\0) 52))

(define (number->base64 n)
  (cond ((< n 26) (integer->char (+ n A0)))
        ((< n 52) (integer->char (+ n a26)))
        ((< n 62) (integer->char (+ n _052)))
        ((equal? n 62) #\-)
        ((equal? n 63) #\_)))

(define (graph-long-name g)
  (list->string (map number->base64 (flatten g))))

(check-equal? (graph-long-name '((0 1) (0 2))) "ABAC")

(define (bits-reorder g)
  (sort
   (map (λ (e) (sort e <)) g)
   (λ (e1 e2)
     (equal?
      -1
      (refine-compare
       (integer-compare (cadr e1) (cadr e2))
       (integer-compare (car e1) (car e2)))))))

; https://users.cecs.anu.edu.au/~bdm/data/formats.txt
(define (graph->bits nb-vertices g)
  (define reordered (bits-reorder g))
  (define bits (make-vector (/ (* nb-vertices (- nb-vertices 1)) 2)))
  (for-each
   (λ (e) (let ((v2 (cadr e)))
            (vector-set! bits (+ (/ (* v2 (- v2 1)) 2)
                                 (car e)) 1)))
   reordered)
  bits)

(check-equal? (graph->bits 3 '((0 1) (0 2))) #(1 1 0))
(check-equal? (graph->bits 4 '((0 1) (0 2) (0 3))) #(1 1 0 1 0 0))
(check-equal? (graph->bits 5 '((0 1) (0 2) (0 3) (0 4))) #(1 1 0 1 0 0 1 0 0 0))

(define (bits->v6s bits)
  (define len (vector-length bits))
  (if (<= len 6)
      (cons (vector-append bits (make-vector (- 6 len))) '())
      (let-values (((head rest) (vector-split-at bits 6)))
        (cons head (bits->v6s rest)))))
                   
(check-equal? (bits->v6s  #(1 1 0 1)) '(#(1 1 0 1 0 0)))
(check-equal? (bits->v6s  #(1 1 0 1 0 0)) '(#(1 1 0 1 0 0)))
(check-equal? (bits->v6s  #(1 1 0 1 0 0 1 0 0 0)) '(#(1 1 0 1 0 0) #(1 0 0 0 0 0)))

(define (v6->number v6)
  (foldl
   (λ (coeff acc) (+ coeff (* 2 acc)))
   0
   (vector->list v6)))

(check-equal? (v6->number #(1 0 0 0 0 0)) 32)
(check-equal? (v6->number #(0 0 0 0 0 1)) 1)

(define (v6->base64-char v6)
  (number->base64 (v6->number v6)))

(check-equal? (v6->base64-char #(1 0 0 0 0 0)) #\g)
(check-equal? (v6->base64-char #(0 0 0 0 0 1)) #\B)


(define (graph->g64 nb-vertices g)
  (list->string (cons (number->base64 nb-vertices)
                      (map v6->base64-char (bits->v6s (graph->bits nb-vertices g))))))

(check-equal? (graph->g64 3 '((0 1) (0 2))) "Dw")
(check-equal? (graph->g64 4 '((0 1) (0 2) (0 3))) "E0")
(check-equal? (graph->g64 5 '((0 1) (0 2) (0 3) (0 4))) "F0g")
(check-equal?
 (graph->g64
  30
  '((0 1) (2 3) (4 5) (6 7) (6 11) (8 9) (10 9) (12 13) (14 13) (14 15) (16 17) (18 15) (0 20) (1 19)
          (2 19) (4 20) (8 22) (10 22) (12 21) (16 20) (18 19) (3 23) (5 23) (7 23) (5 27) (7 11)
          (24 25) (24 26) (3 21) (24 20) (25 27) (26 27) (28 29) (9 15) (25 13) (27 13) (28 17)
          (29 13) (29 15) (29 17) (9 21) (11 21) (11 22) (25 22) (26 21) (26 22) (28 20) (28 21)
          (15 17) (17 19) (19 20) (19 22)))
 "ehAIAEAAIAgYAAAAgAIAhAAAAAYAAjAAOIAJEFgABYCCoAAAAAIAAQCgAADQICADAABMAABUAI")
(check-equal?
 (string-length (graph->g64
  30
  '((0 1) (2 3) (4 5) (6 7) (6 11) (8 9) (10 9) (12 13) (14 13) (14 15) (16 17) (18 15) (0 20) (1 19)
          (2 19) (4 20) (8 22) (10 22) (12 21) (16 20) (18 19) (3 23) (5 23) (7 23) (5 27) (7 11)
          (24 25) (24 26) (3 21) (24 20) (25 27) (26 27) (28 29) (9 15) (25 13) (27 13) (28 17)
          (29 13) (29 15) (29 17) (9 21) (11 21) (11 22) (25 22) (26 21) (26 22) (28 20) (28 21)
          (15 17) (17 19) (19 20) (19 22))))
 74)

(define (graph->md5-64 g)
  (foldl
   (λ (e acc) (string-replace acc (car e) (cdr e)))
   (bytes->string/utf-8 (base64-encode (md5 (~a g) #f)))
   (list (cons "+" "-") (cons "/" "_") (cons "=" "") (cons "\r" "") (cons "\n" ""))))

(check-equal?
 (graph->md5-64
  '((0 1) (2 3) (4 5) (6 7) (6 11) (8 9) (10 9) (12 13) (14 13) (14 15) (16 17) (18 15) (0 20) (1 19)
          (2 19) (4 20) (8 22) (10 22) (12 21) (16 20) (18 19) (3 23) (5 23) (7 23) (5 27) (7 11)
          (24 25) (24 26) (3 21) (24 20) (25 27) (26 27) (28 29) (9 15) (25 13) (27 13) (28 17)
          (29 13) (29 15) (29 17) (9 21) (11 21) (11 22) (25 22) (26 21) (26 22) (28 20) (28 21)
          (15 17) (17 19) (19 20) (19 22)))
 "fuD_Gbyj9B60EZoT5uQRBQ")

(define (make-directory-and-parents dir)
  (when (not (directory-exists? dir))
    (make-directory-and-parents (simplify-path (build-path dir 'up)))
    (make-directory dir)))

(define (write-dot-file g dir vertices)
  (define md5 (graph->md5-64 g))
  (define graph-dir (~a dir "/" md5))
  (when (not (directory-exists? graph-dir))
      (make-directory-and-parents graph-dir))
  (with-output-to-file (~a graph-dir "/_.dot")
    (λ() (printf (graph-dot g vertices))))
  (with-output-to-file (~a graph-dir "/_.js")
    (λ() (display "export const nextss = ")
      (write-json (get-graph-nextss g (length vertices)))))
  graph-dir)

(define (random-edge-from vertices vertex)
  (list vertex (random-ref (remove vertex vertices))))

(define (random-edge vertices)
  (random-edge-from vertices (random-ref vertices)))

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


(define (make-all-vertices-degree2 graph vertices)
  (define result (append graph
                         (map (λ (vertex) (random-edge-from vertices vertex))
                              (get-graph-degree-1-vertices graph vertices))))
  (when (not (null? (get-graph-degree-1-vertices result vertices)))
    (raise (list graph '() '() result)))
  result)

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
  (range 30)))


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
(define (get-new-degrees edges degrees first-vertex)
  (foldl (λ (i degs) (list-update degs i (λ (d) (- d 1))))
         degrees
         (map (λ (v) (- v first-vertex 1)) (map cadr edges))))

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
  (define sub-graphs (degrees->graphs new-degrees
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
(define (degrees->graphs degrees first-vertex all-categories first-vertex-categories)
  (define length-degrees (length degrees))
  (cond ((equal? degrees '())
         '())
        ((null? (car all-categories))
         (degrees->graphs degrees first-vertex (cdr all-categories) first-vertex-categories))
        ((null? (car first-vertex-categories))
         (degrees->graphs degrees first-vertex all-categories (cdr first-vertex-categories)))
        ((equal? degrees '(0))
         '(())) ; the empty graph
        ((memf (λ (deg) (>= deg length-degrees)) degrees)
         '())
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
        ((null? (car all-categories))
         (rec-degrees->graphs-stream degrees first-vertex (cdr all-categories) first-vertex-categories))
        ((null? (car first-vertex-categories))
         (rec-degrees->graphs-stream degrees first-vertex all-categories (cdr first-vertex-categories)))
        ((equal? degrees '(0))
          (stream '())) ; the empty graph
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

(define (get-degrees-categories degrees first-vertex)
  (let* ((first-degree (car degrees))
         (index (index-where degrees (λ (d) (not (equal? d first-degree))))))
    (if index
        (cons (range first-vertex (+ first-vertex index))
              (get-degrees-categories (drop degrees index) (+ first-vertex index)))
        (list (range first-vertex (+ first-vertex (length degrees)))))))

(check-equal? (get-degrees-categories '(2 2 3 4 4 5) 0)
              '((0 1) (2) (3 4) (5)))

(define (degrees->graphs-stream degrees)
  (define categories (get-degrees-categories degrees 0))
  (rec-degrees->graphs-stream degrees 0 categories categories))

(check-equal? (degrees->graphs '(0 2) 3 '((4)) '((4))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream  '(0 2) 3 '((4)) '((4))))
              (degrees->graphs '(0 2) 3 '((4)) '((4))))
(check-equal? (degrees->graphs '(0) 1 '((1)) '((1))) '(()))
(check-equal? (stream->list (rec-degrees->graphs-stream  '(0) 1 '((1)) '((1))))
              (degrees->graphs '(0) 1 '((1)) '((1))))

(check-equal? (get-new-new-categories '((1)) '((0 1))) '((1)))
(check-equal? (get-new-degrees '((0 1)) '(1) 0) '(0))
(check-equal? (rec-parts-w/nb-categories '(((0 1))) 1) '(((0 1))))
(check-equal? (get-edge-categories 0 '((1))) '(((0 1))))
(check-equal? (filter-out-vertex-from-categories 0 '((0 1))) '((1)))
(check-equal? (degrees->graphs '(1 1) 0 '((0 1)) '((0 1))) '(((0 1)))) ; 0--1
(check-equal? (stream->list (rec-degrees->graphs-stream  '(0) 1 '((1)) '((1))))
              (degrees->graphs '(0) 1 '((1)) '((1))))

(check-equal? (degrees->graphs '(0) 3 '((3)) '((3))) '(()))
(check-equal? (stream->list (rec-degrees->graphs-stream  '(0) 3 '((3)) '((3))))
              (degrees->graphs '(0) 3 '((3)) '((3))))

(check-equal? (get-new-new-categories '((3)) '((2 3))) '((3)))
(check-equal? (get-new-degrees '((2 3)) '(1) 2) '(0))
(check-equal? (rec-parts-w/nb-categories '(((2 3))) 1) '(((2 3))))
(check-equal? (get-edge-categories 2 '((3))) '(((2 3))))
(check-equal? (filter-out-vertex-from-categories 2 '((2 3))) '((3)))
(check-equal? (degrees->graphs '(1 1) 2 '((2 3)) '((2 3))) '(((2 3))))
(check-equal? (stream->list (rec-degrees->graphs-stream  '(1 1) 2 '((2 3)) '((2 3))))
              (degrees->graphs '(1 1) 2 '((2 3)) '((2 3))))

(check-equal? (get-new-new-categories '((2 3)) '((1 2) (1 3))) '((2 3)))
(check-equal? (get-new-degrees '((1 2) (1 3)) '(2 2) 1) '(1 1))
(check-equal? (rec-parts-w/nb-categories '(((1 2) (1 3))) 2) '(((1 2) (1 3))))
(check-equal? (get-edge-categories 1 '((2 3))) '(((1 2) (1 3))))
(check-equal? (filter-out-vertex-from-categories 1 '((1) (2 3))) '((2 3)))
(check-equal? (degrees->graphs '(2 2 2) 1 '((1) (2 3)) '((1) (2 3))) '(((1 2) (1 3) (2 3))))
(check-equal? (stream->list (rec-degrees->graphs-stream  '(2 2 2) 1 '((1) (2 3)) '((1) (2 3))))
              (degrees->graphs '(2 2 2) 1 '((1) (2 3)) '((1) (2 3))))

(check-equal? (get-new-new-categories '((1) (2 3)) '((0 2) (0 3))) '((1) (2 3)))
(check-equal? (get-new-degrees '((0 2) (0 3)) '(2 3 3) 0) '(2 2 2))

(check-equal? (rec-parts-w/nb-categories '() '(2)) '())
(check-equal? (get-edge-categories 3 '()) '())
(check-equal? (filter-out-vertex-from-categories 3 '((3))) '())
(check-equal? (degrees->graphs '(2) 3 '((3)) '((3))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream  '(2) 3 '((3)) '((3))))
              (degrees->graphs '(2) 3 '((3)) '((3))))

(check-equal? (get-new-new-categories '((3)) '((2 3))) '((3)))
(check-equal? (get-new-degrees '((2 3)) '(3) 2) '(2))
(check-equal? (rec-parts-w/nb-categories '(((2 3))) 1) '(((2 3))))
(check-equal? (get-edge-categories 2 '((3))) '(((2 3))))
(check-equal? (filter-out-vertex-from-categories 2 '((2) (3))) '((3)))
(check-equal? (degrees->graphs '(1 3) 2 '((2) (3)) '((2) (3))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream  '(1 3) 2 '((2) (3)) '((2) (3))))
              (degrees->graphs '(1 3) 2 '((2) (3)) '((2) (3))))

(check-equal? (get-new-new-categories '((2) (3)) '((1 2))) '((2) (3)))
(check-equal? (get-new-degrees '((1 2)) '(2 3) 1) '(1 3))
(check-equal? (rec-parts-w/nb-categories '(((1 2)) ((1 3))) 1) '(((1 2)) ((1 3))))
(check-equal? (get-edge-categories 1 '((2) (3))) '(((1 2)) ((1 3))))
(check-equal? (filter-out-vertex-from-categories 1 '((1) (2) (3))) '((2) (3)))
(check-equal? (degrees->graphs '(1 2 3) 1 '((1) (2) (3)) '((1) (2) (3))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream   '(1 2 3) 1 '((1) (2) (3)) '((1) (2) (3))))
              (degrees->graphs  '(1 2 3) 1 '((1) (2) (3)) '((1) (2) (3))))

(check-equal? (get-new-new-categories '((1) (2 3)) '((0 1) (0 2))) '((1) (2) (3)))
(check-equal? (get-new-degrees '((0 1) (0 2)) '(2 3 3) 0) '(1 2 3))
(check-equal? (rec-parts-w/nb-categories '(((0 1)) ((0 2) (0 3))) 2) '(((0 1) (0 2)) ((0 2) (0 3))))
(check-equal? (get-edge-categories 0 '((1) (2 3))) '(((0 1)) ((0 2) (0 3))))
(check-equal? (filter-out-vertex-from-categories 0 '((0 1) (2 3))) '((1) (2 3)))
(check-equal? (degrees->graphs '(2 2 3 3) 0 '((0 1) (2 3)) '((0 1) (2 3))) '(((0 2) (0 3) (1 2) (1 3) (2 3))))
(check-equal? (stream->list (rec-degrees->graphs-stream  '(2 2 3 3) 0 '((0 1) (2 3)) '((0 1) (2 3))))
              (degrees->graphs '(2 2 3 3) 0 '((0 1) (2 3)) '((0 1) (2 3))))


(check-equal? (degrees->graphs '(1) 4 '((4)) '((4))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream  '(1) 4 '((4)) '((4))))
              (degrees->graphs '(1) 4 '((4)) '((4))))
(check-equal? (get-new-new-categories '((4)) '([3 4])) '((4)))
(check-equal? (remove-categories-before-first-joined '((4)) '([3 4])) '((4)))
(check-equal? (get-new-degrees '([3 4]) '(2) 3) '(1))
(check-equal? (rec-parts-w/nb-categories '(([3 4])) 0) '(()))
(check-equal? (get-edge-categories 3 '((4))) '(([3 4])))
(check-equal? (filter-out-vertex-from-categories 3 '((4))) '((4)))
(check-equal? (degrees->graphs '(0 2) 3 '((4)) '((4))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream '(0 2) 3 '((4)) '((4))))
              (degrees->graphs '(0 2) 3 '((4)) '((4))))
(check-equal? (get-new-new-categories '((4)) '([2 3] [2 4])) '((4)))
(check-equal? (remove-categories-before-first-joined '((3) (4)) '([2 3] [2 4])) '((3) (4)))
(check-equal? (get-new-degrees '([2 3] [2 4]) '(1 3) 2) '(0 2))
(check-equal? (rec-parts-w/nb-categories  '(([2 3]) ([2 4])) 2) '(([2 3] [2 4])))
(check-equal? (get-edge-categories 2 '((3) (4))) '(([2 3]) ([2 4])))
(check-equal? (filter-out-vertex-from-categories 2 '((2) (3) (4))) '((3) (4)))
(check-equal? (degrees->graphs '(2 1 3) 2 '((2) (3) (4)) '((2) (3) (4))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 1 3) 2 '((2) (3) (4)) '((2) (3) (4))))
              (degrees->graphs '(2 1 3) 2 '((2) (3) (4)) '((2) (3) (4))))
(check-equal? (get-new-new-categories '((3) (4)) '([1 3])) '((3) (4)))
(check-equal? (remove-categories-before-first-joined '((2) (3) (4)) '([1 3])) '((3) (4)))
(check-equal? (get-new-degrees '([1 3]) '(2 2 3) 1) '(2 1 3))
              
(check-equal? (degrees->graphs '(1 2 3) 2 '((2) (3) (4)) '((2) (3) (4))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream '(1 2 3) 2 '((2) (3) (4)) '((2) (3) (4))))
              (degrees->graphs '(1 2 3) 2 '((2) (3) (4)) '((2) (3) (4))))
(check-equal? (get-new-new-categories '((2) (3) (4)) '([1 2])) '((2) (3) (4)))
(check-equal? (remove-categories-before-first-joined '((2) (3) (4)) '([1 2])) '((2) (3) (4)))
(check-equal? (get-new-degrees '([1 2]) '(2 2 3) 1) '(1 2 3))

(check-equal? (rec-parts-w/nb-categories  '(([1 2]) ([1 3]) ([1 4])) 1) '(([1 2]) ([1 3]) ([1 4])))
(check-equal? (get-edge-categories 1  '((2) (3) (4)))  '(([1 2]) ([1 3]) ([1 4])))
(check-equal? (filter-out-vertex-from-categories 1 '((1) (2) (3) (4))) '((2) (3) (4)))

(check-equal? (degrees->graphs '(2 2 2) 2 '((2 3) (4)) '((4))) '())
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 2) 2 '((2 3) (4)) '((4))))
              (degrees->graphs '(2 2 2) 2 '((2 3) (4)) '((4))))

(check-equal? (degrees->graphs '(1 2 2 3) 1 '((1) (2) (3) (4)) '((1) (2) (3) (4))) '({(1 4) (2 3) (2 4) (3 4)}))
(check-equal? (stream->list (rec-degrees->graphs-stream '(1 2 2 3) 1 '((1) (2) (3) (4)) '((1) (2) (3) (4))))
              (degrees->graphs '(1 2 2 3) 1 '((1) (2) (3) (4)) '((1) (2) (3) (4))))

(check-equal? (degrees->graphs '(1 2 2 3) 1 '((1) (2 3) (4)) '((1) (2 3) (4))) '({(1 4) (2 3) (2 4) (3 4)}))
(check-equal? (stream->list (rec-degrees->graphs-stream '(1 2 2 3) 1 '((1) (2 3) (4)) '((1) (2 3) (4))))
              (degrees->graphs '(1 2 2 3) 1 '((1) (2 3) (4)) '((1) (2 3) (4))))

(check-equal? (get-new-new-categories '((1 2) (3 4)) '([0 1] [0 3])) '((1) (2) (3) (4)))
(check-equal? (get-new-degrees '([0 1] [0 3]) '(2 2 3 3) 0) '(1 2 2 3))
            
(check-equal? (degrees->graphs '(1 1 3 3) 1 '((1 2) (3 4)) '((1 2) (3 4))) '())
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
(check-equal? (degrees->graphs '(2 2 2 3 3) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4)))
              '(((0 1) (0 3) (1 4) (2 3) (2 4) (3 4))
                ((0 3) (0 4) (1 3) (1 4) (2 3) (2 4))))
(check-equal? (stream->list (degrees->graphs-stream '(2 2 2 3 3)))
              (degrees->graphs '(2 2 2 3 3) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4))))

; 0--1--3--4--2
; `-----------'
(check-equal? (degrees->graphs '(2 2 2 2 2) 0 '((0 1 2 3 4)) '((0 1 2 3 4)))
              '(((0 1) (0 2) (1 3) (2 4) (3 4))))
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 2 2 2) 0 '((0 1 2 3 4)) '((0 1 2 3 4))))
              (degrees->graphs '(2 2 2 2 2) 0 '((0 1 2 3 4)) '((0 1 2 3 4))))

; 0   2
; |\ /|
; | 4 |
; |/ \|
; 1   3
(check-equal? (degrees->graphs '(2 2 2 2 4) 0 '((0 1 2 3) (4)) '((0 1 2 3) (4)))
              '(((0 1) (0 4) (1 4) (2 3) (2 4) (3 4))))
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 2 2 4) 0 '((0 1 2 3) (4)) '((0 1 2 3) (4))))
              (degrees->graphs '(2 2 2 2 4) 0 '((0 1 2 3) (4)) '((0 1 2 3) (4))))


; 0--3--2
;  \/| /
;  /\|/
; 1--4
(check-equal? (degrees->graphs '(2 2 2 4 4) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4)))
              '(((0 3) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4))))
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 2 4 4) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4))))
              (degrees->graphs '(2 2 2 4 4) 0 '((0 1 2) (3 4)) '((0 1 2) (3 4))))


; 0--2---3--1
; |   \ /   |
; `----4----'
(check-equal? (degrees->graphs '(2 2 3 3 4) 0 '((0 1) (2 3) (4)) '((0 1) (2 3) (4)))
              '(((0 2) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4))))
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 3 3 4) 0 '((0 1) (2 3) (4)) '((0 1) (2 3) (4))))
              (degrees->graphs '(2 2 3 3 4) 0 '((0 1) (2 3) (4)) '((0 1) (2 3) (4))))

; 0
; |\
; 1 2---3
; |  \ /
; `---4
(check-equal? (degrees->graphs '(2 3 3 3 3) 0 '((0) (1 2 3 4)) '((0) (1 2 3 4)))
              '(((0 1) (0 2) (1 3) (1 4) (2 3) (2 4) (3 4))))
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 3 3 3 3) 0 '((0) (1 2 3 4)) '((0) (1 2 3 4))))
              (degrees->graphs '(2 3 3 3 3) 0 '((0) (1 2 3 4)) '((0) (1 2 3 4))))

(check-equal? (degrees->graphs '(2 2 4 4 4) 0 '((0 1) (2 3 4)) '((0 1) (2 3 4)))
              '())
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 2 4 4 4) 0 '((0 1) (2 3 4)) '((0 1) (2 3 4))))
              (degrees->graphs '(2 2 4 4 4) 0 '((0 1) (2 3 4)) '((0 1) (2 3 4))))

;     0
;    / \
;   3---4
;   |\ /|
;   | X |
;   |/ \|
;   1---2
(check-equal? (degrees->graphs '(2 3 3 4 4) 0 '((0) (1 2) (3 4)) '((0) (1 2) (3 4)))
              '(((0 3) (0 4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4))))
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 3 3 4 4) 0 '((0) (1 2) (3 4)) '((0) (1 2) (3 4))))
              (degrees->graphs '(2 3 3 4 4) 0 '((0) (1 2) (3 4)) '((0) (1 2) (3 4))))

; ,---------.
; 0--2---3--1
; |   \ /   |
; `----4----'
(check-equal? (degrees->graphs '(3 3 3 3 4) 0 '((0 1 2 3) (4)) '((0 1 2 3) (4)))
              '(((0 1) (0 2) (0 4) (1 3) (1 4) (2 3) (2 4) (3 4))))
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 3 3 4 4) 0 '((0) (1 2) (3 4)) '((0) (1 2) (3 4))))
              (degrees->graphs '(2 3 3 4 4) 0 '((0) (1 2) (3 4)) '((0) (1 2) (3 4))))

(check-equal? (degrees->graphs '(2 4 4 4 4) 0 '((0) (1 2 3 4)) '((0) (1 2 3 4)))
              '())
(check-equal? (stream->list (rec-degrees->graphs-stream '(2 4 4 4 4) 0 '((0) (1 2 3 4)) '((0) (1 2 3 4))))
              (degrees->graphs '(2 4 4 4 4) 0 '((0) (1 2 3 4)) '((0) (1 2 3 4))))


 

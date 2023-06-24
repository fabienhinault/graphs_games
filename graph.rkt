#lang racket

(require rackunit)
(require srfi/67) ; compare procedures

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

(check-equal? (tailrec-couples '() '(a z e r)) '((a z) (a e) (a r) (z e) (z r) (e r)) "tailrec-couples")
(check-equal? (tailrec-couples '() '(a e r z)) '((a e) (a r) (a z) (e r) (e z) (r z)) "tailrec-couples")

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

(check-equal? (tailrec-sorted-couples '() '(a z e r) symbol<?) '((a z) (a e) (a r) (e z) (r z) (e r)) "tailrec-sorted-couples")
(check-equal? (tailrec-sorted-couples '() '(a e r z) symbol<?) '((a e) (a r) (a z) (e r) (e z) (r z)) "tailrec-sorted-couples")

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

(define (graphs1 l)
  (sort (tailrec-sorted-parts '(()) (tailrec-sorted-couples '() l symbol<?) edge<?) graph<?))

(check-equal?
 (graphs1 '(a e z))
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

(define (add-uniq s l)
  (if (member s l)
      l
      (cons s l)))

(check-equal? (add-uniq 'a '()) '(a))
(check-equal? (add-uniq 'a '(a)) '(a))
(check-equal? (add-uniq 'b '(a)) '(b a))
(check-equal? (add-uniq 'r '(e z a)) '(r e z a))

; xs and ys are lists of unique symbols, but they can share some symbols
; the use case is: ys contains symbols from (cdr l) in reverse order
; xs contains symbols from (car l) in reverse order and we want as
; result the symbols of l in reverse order.
(define (append-uniq xs ys)
  (foldl add-uniq xs ys))

(check-equal? (append-uniq '(a) '()) '(a))
(check-equal? (append-uniq '(a) '(a)) '(a))
(check-equal? (append-uniq '(a) '(b)) '(b a))
(check-equal? (append-uniq '(z a) '(e a)) '(e z a))
(check-equal? (append-uniq '(r e z a) '(t e z a)) '(t r e z a))


(define (get-uniqs l)
  (cond ((null? l) '())
        ((pair? l) (append-uniq (get-uniqs (car l)) (get-uniqs (cdr l))))
        (else (list l))))

(check-equal? (get-uniqs 'a) '(a))
(check-equal? (get-uniqs '(a)) '(a))
(check-equal? (get-uniqs '(a z)) '(z a))
(check-equal? (get-uniqs '((a e) (a z)))
              '(z e a))
(check-equal? (get-uniqs '((z e) (a e) (a z)))
              '(a e z))

               
;(define (update-res-topo-graphs res l)

(define (topo-graph l)
    (let ((syms (map (λ (_) (gensym)) (range (length l)))))
      (map
       (λ (graph)
         (let ((uniqs (get-uniqs graph)))
           (foldl replace-all-deep graph (reverse uniqs) (take l (length uniqs)))))
       (graphs1 syms))))

(check-equal? (topo-graph '(a e z))
              '(() ((a e)) ((a e)) ((a e)) ((a e) (a z)) ((a e) (e z)) ((a e) (z e)) ((a e) (a z) (e z))))

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

(let* ((G '((a z) (a e)))
      (v<? (vertice<?* G)))
  (check-true (v<? 'e 'a))
  (check-true (v<? 'z 'a))
  (check-false (v<? 'a 'z))
  (check-false (v<? 'a 'e))
  (check-true (v<? 'e 'z))
  (check-false (v<? 'a 'e)))
  
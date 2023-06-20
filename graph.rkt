#lang racket

(require rackunit)

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

(define (graphs1 l)
  (tailrec-parts '(()) (tailrec-couples '() l)))

(check-equal?
 (graphs1 '(a z e))
'(() ((a z)) ((a e)) ((a e) (a z)) ((z e)) ((z e) (a z)) ((z e) (a e)) ((z e) (a e) (a z))))

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
(define (append-uniq xs ys)
  (foldl add-uniq xs ys))

(check-equal? (append-uniq '(a) '()) '(a))
(check-equal? (append-uniq '(a) '(a)) '(a))
(check-equal? (append-uniq '(a) '(b)) '(b a))
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
      (foldl replace-all-deep
             (graphs1 syms)
             syms
             l)))

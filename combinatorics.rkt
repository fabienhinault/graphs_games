#lang racket


(define (rec-couples l)
    (if (null? l)
        '()
        (append
         (map
          (lambda (_) (list (car l) _))
          (cdr l))
         (rec-couples (cdr l)))))

(module+ test
  (require rackunit)
  (check-equal? (rec-couples '(a z e r)) '((a z) (a e) (a r) (z e) (z r) (e r)) "rec_couples"))

(define (tailrec-couples res l)
    (if (null? l)
        res
        (tailrec-couples
         (append
          res
          (map (lambda (_) (list (car l) _))
               (cdr l)))
         (cdr l))))

(module+ test
  (check-equal? (tailrec-couples '() '(a z e r))
                '((a z) (a e) (a r) (z e) (z r) (e r)) "tailrec-couples")
  (check-equal? (tailrec-couples '() '(a e r z))
                '((a e) (a r) (a z) (e r) (e z) (r z)) "tailrec-couples"))

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

(module+ test
  (check-equal? (tailrec-sorted-couples '() '(a z e r) symbol<?)
                '((a z) (a e) (a r) (e z) (r z) (e r)) "tailrec-sorted-couples")
  (check-equal? (tailrec-sorted-couples '() '(a e r z) symbol<?)
                '((a e) (a r) (a z) (e r) (e z) (r z)) "tailrec-sorted-couples"))

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

(module+ test
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
                                         ())))

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

(module+ test
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
  (check-equal? (rec-parts-w/nb '(0 1 2 3) 4) '((0 1 2 3))))


; partitions of (apply append categories) having nb elements,
; always taking the first elements in each category
(define (rec-parts-w/nb-categories categories nb)
    (cond ((null? categories)
           '())
          ((equal? 0 nb)
           '(()))
          ((null? (car categories))
           (rec-parts-w/nb-categories (cdr categories) nb))
          (else
           ; since we only take the first elements of each category,
           ; in (car categories) we only take (caar categories).
           ; so the results the append of
           ; - the parts beginning with (caar categories) and
           ; - the parts not containing (car categories)
           (append
            (map (λ (_) (cons (caar categories) _))
                 (rec-parts-w/nb-categories (cons (cdar categories) (cdr categories)) (- nb 1)))
            (rec-parts-w/nb-categories (cdr categories) nb)))))

(module+ test
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
  (check-equal? (rec-parts-w/nb-categories '(([1 2]) ([1 3])) 1) '(([1 2]) ([1 3]))))

; stream version
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

(define (tailrec-parts-update-result tmp-res x)
    (append
     tmp-res
     (map
      (λ (_) (cons x _))
      tmp-res)))

(module+ test
  (check-equal? (tailrec-parts-update-result '((e) ()) 'z) '((e) () (z e) (z))))

(define (tailrec-parts res l)
    (if (null? l)
        res
        (tailrec-parts
         (tailrec-parts-update-result res (car l))
         (cdr l))))

(module+ test
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
                                                   (r e z a))))

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

(module+ test
  (check-equal? (tailrec-parts-w/nb-update-result '((())) 0 1) '((())) )
  (check-equal? (tailrec-parts-w/nb-update-result '((()) ()) 0 2) '((()) ((0))) ))
 
(define (tailrec-parts-w/nb res l nb+1)
    (if (null? l)
        (last res)
        (tailrec-parts-w/nb
         (tailrec-parts-w/nb-update-result res (car l) nb+1)
         (cdr l)
         nb+1)))

(module+ test
  (check-equal? (tailrec-parts-w/nb '((()) ()) '(0) 2) '((0))))

(define (parts-w/nb l nb)
  (tailrec-parts-w/nb (cons '(()) (make-list nb '())) l (+ nb 1)))

(module+ test
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
  (check-equal? (parts-w/nb '(0 1 2 3) 4) '((3 2 1 0))))

(provide
 tailrec-couples
 tailrec-parts
 parts-w/nb
 rec-parts-w/nb-categories
 rec-parts-w/nb-categories-stream
 next-part/nb-categories
 first-part/nb-categories)
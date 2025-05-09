#lang racket

(require "combinatorics.rkt")

(define (get-maxes categories filled-slot)
  (define raw-maxes (map category-max categories))
  (if (null? raw-maxes)
      '()
      (cons (- (car raw-maxes) filled-slot) (cdr raw-maxes))))

(module+ test
  (require rackunit)
  (check-equal? (get-maxes '(#s(category ((2 3 4)) 1.0) #s(category ((5)) +inf.0)) 1)
                '(0.0 +inf.0))
  (check-equal? (get-maxes '(#s(category ((2 3 4))  +inf.0) #s(category ((5)) +inf.0)) 1)
                '(+inf.0 +inf.0)))


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
  (check-equal? (get-new-degrees '((0 1) (0 2)) '(2 3 3) 0) '(1 2 3))
  (check-equal? (get-new-degrees '((0 1) (0 5)) '(2 2 2 2 4) 0) '(1 2 2 2 3)))



(define (rnbs->rmaxes rnbs)
  (cond ((null? rnbs)
         '())
        ((equal? 0 (car rnbs))
         (cons +inf.0 (rnbs->rmaxes (cdr rnbs))))
        (else
         (cons +inf.0 (cdr rnbs)))))

; in: nbs            number of vertices the start vertex is edging to in each category
; in: filled-slot    number of previous vertices of the first category
; the starting vertex was already edging to
; return:            maxes to apply to categories according to these numbers
(define (nbs->maxes nbs filled-slot)
  (define nbs/slot (cons (+ filled-slot (car nbs)) (cdr nbs)))
  (reverse (rnbs->rmaxes (reverse nbs/slot))))

(define (get-nbs-categories category-nbs new-v1-categories filled-slot)
  (define maxes (nbs->maxes category-nbs filled-slot))
  (map (λ (cat new-max) (category (category-verticess cat) new-max))
       new-v1-categories
       maxes))


(module+ test
  (check-equal? (get-nbs-categories  '(1 1) '(#s(category ((1 2 3 4)) +inf.0) #s(category ((5)) +inf.0)) 0)
                                     '(#s(category ((1 2 3 4)) 1) #s(category ((5)) +inf.0)))
  (check-equal? (get-nbs-categories  '(2 0) '(#s(category ((1 2 3 4)) +inf.0) #s(category ((5)) +inf.0)) 0)
                                     '(#s(category ((1 2 3 4)) +inf.0) #s(category ((5)) +inf.0)))
  (check-equal? (get-nbs-categories  '(0 1) '(#s(category ((2 3 4)) +inf.0) #s(category ((5)) +inf.0)) 1)
                                     '(#s(category ((2 3 4)) 1) #s(category ((5)) +inf.0)))
  (check-equal?
   (get-nbs-categories
    '(0 1 1)
    '(#s(category ((3)) 1.0) #s(category ((4)) 0.0) #s(category ((5)) +inf.0)) 0)
   '(#s(category ((3)) 0) #s(category ((4)) 1) #s(category ((5)) +inf.0))))


(provide
 get-maxes
 get-new-edgess
 get-new-degrees
 nbs->maxes
 get-nbs-categories)
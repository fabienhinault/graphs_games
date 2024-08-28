#lang racket

(require (for-syntax srfi/67)) ; compare procedures
(require srfi/67)
(require "graph-utils.rkt")

(define-for-syntax (compare-to< compare)
  (λ (_1 _2) (< 0 (compare _1 _2))))

(define-for-syntax (<to-compare <)
  (λ (_1 _2)
    (cond ((< _1 _2) -1)
          ((< _2 _1) 1)
          (else 0))))

(define-syntax (refine< stx)
  (syntax-case stx ()
    ((_ < ...)
     #`(compare-to< (refine-compare (<to-compare <) ...)))))

(define (deep f l)
    (cond ((null? l) '())
          ((pair? l) (cons (deep f (car l)) (deep f (cdr l))))
          (else (f l))))


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
  

(module+ test
  (require rackunit)
  (let* ((G '((a z) (a e)))
         (v<? (vertice<?* G)))
    (check-true (v<? 'e 'a))
    (check-true (v<? 'z 'a))
    (check-false (v<? 'a 'z))
    (check-false (v<? 'a 'e))
    (check-true (v<? 'e 'z))
    (check-false (v<? 'a 'e))))

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

(module+ test
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
    (check-true  (e<? '(3 4) '(2 4)))))
  
(define (edge-compare* graph)
  (let ((get-vertex-degree (λ (_) (get-degree _ graph))))
    (λ (e1 e2)
      (refine-compare
          (integer-compare (get-vertex-degree (car e1)) (get-vertex-degree (car e2))
          (integer-compare (get-vertex-degree (cadr e1)) (get-vertex-degree (cadr e2))
          (symbol-compare (car e1) (car e2))
          (symbol-compare (cadr e1) (cadr e2))))))))

(provide edge<?* refine<)
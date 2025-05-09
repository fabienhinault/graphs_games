#lang racket

(require srfi/67)
(require net/base64)
(require file/md5)
(require json)

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

(module+ test
  (require rackunit)
  (check-equal? (graph-long-name '((0 1) (0 2))) "ABAC"))

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

(module+ test
  (check-equal? (graph->bits 3 '((0 1) (0 2))) #(1 1 0))
  (check-equal? (graph->bits 4 '((0 1) (0 2) (0 3))) #(1 1 0 1 0 0))
  (check-equal? (graph->bits 5 '((0 1) (0 2) (0 3) (0 4))) #(1 1 0 1 0 0 1 0 0 0)))

(define (bits->v6s bits)
  (define len (vector-length bits))
  (if (<= len 6)
      (cons (vector-append bits (make-vector (- 6 len))) '())
      (let-values (((head rest) (vector-split-at bits 6)))
        (cons head (bits->v6s rest)))))
      
(module+ test             
  (check-equal? (bits->v6s  #(1 1 0 1)) '(#(1 1 0 1 0 0)))
  (check-equal? (bits->v6s  #(1 1 0 1 0 0)) '(#(1 1 0 1 0 0)))
  (check-equal? (bits->v6s  #(1 1 0 1 0 0 1 0 0 0)) '(#(1 1 0 1 0 0) #(1 0 0 0 0 0))))

(define (v6->number v6)
  (foldl
   (λ (coeff acc) (+ coeff (* 2 acc)))
   0
   (vector->list v6)))

(module+ test
  (check-equal? (v6->number #(1 0 0 0 0 0)) 32)
  (check-equal? (v6->number #(0 0 0 0 0 1)) 1))

(define (v6->base64-char v6)
  (number->base64 (v6->number v6)))

(module+ test
  (check-equal? (v6->base64-char #(1 0 0 0 0 0)) #\g)
  (check-equal? (v6->base64-char #(0 0 0 0 0 1)) #\B))


(define (graph->g64 nb-vertices g)
  (list->string (cons (number->base64 nb-vertices)
                      (map v6->base64-char (bits->v6s (graph->bits nb-vertices g))))))

(module+ test
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
   74))


(define (graph->md5-64 g)
  (define b64+/= (bytes->string/utf-8 (base64-encode (md5 (~a g) #f))))
  (foldl
   (λ (replacement acc) (string-replace acc (car replacement) (cdr replacement)))
   b64+/=
   (list (cons "+" "-") (cons "/" "_") (cons "=" "") (cons "\r" "") (cons "\n" ""))))

(module+ test
  (check-equal?
   (graph->md5-64
    '((0 1) (2 3) (4 5) (6 7) (6 11) (8 9) (10 9) (12 13) (14 13) (14 15) (16 17) (18 15) (0 20) (1 19)
            (2 19) (4 20) (8 22) (10 22) (12 21) (16 20) (18 19) (3 23) (5 23) (7 23) (5 27) (7 11)
            (24 25) (24 26) (3 21) (24 20) (25 27) (26 27) (28 29) (9 15) (25 13) (27 13) (28 17)
            (29 13) (29 15) (29 17) (9 21) (11 21) (11 22) (25 22) (26 21) (26 22) (28 20) (28 21)
            (15 17) (17 19) (19 20) (19 22)))
   "fuD_Gbyj9B60EZoT5uQRBQ"))


(define (vertex-dot v)
    (~a v " [id=\"id" v "\"]" #\newline))

(define (edge-dot e)
  (~a (car e) " -- " (cadr e) " [class=\"_" (car e) "_ _" (cadr e)  "_\"]" #\newline))

(module+ test
  (check-equal?
   (edge-dot '(a z)) "a -- z [class=\"_a_ _z_\"]
"))

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


; util function for get-graph-nextss
(define (add-edge-to-graph-vector edge acc-vector-nextss)
  (vector-set! acc-vector-nextss (car edge)
               (cons (cadr edge) (vector-ref acc-vector-nextss (car edge))))
  (vector-set! acc-vector-nextss (cadr edge)
               (cons (car edge) (vector-ref acc-vector-nextss (cadr edge))))
  acc-vector-nextss)

; for each vertex, the list of adjascent vertices
(define (get-graph-nextss graph nb-vertices)
  (vector->list (foldl add-edge-to-graph-vector (make-vector nb-vertices '()) graph)))

(module+ test
  (require rackunit)
  (check-equal? (get-graph-nextss '((0 2) (1 2)) 3) '((2) (2) (1 0))))

(define (make-directory-and-parents dir)
  (when (not (directory-exists? dir))
    (make-directory-and-parents (simplify-path (build-path dir 'up)))
    (make-directory dir)))


(define (write-dot-file g dir vertices)
  (define md5 (graph->md5-64 g))
  (define graph-dir (~a dir "/" md5))
  (when (not (directory-exists? graph-dir))
      (make-directory-and-parents graph-dir))
  (with-output-to-file (~a graph-dir "/" md5 ".dot")
    (λ() (printf (graph-dot g vertices))))
  (with-output-to-file (~a graph-dir "/" md5 ".js")
    (λ() (display "export const nextss = ")
      (write-json (get-graph-nextss g (length vertices)))))
  graph-dir)


(provide graph->md5-64 write-dot-file)
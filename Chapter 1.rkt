#lang eopl
(require test-engine/racket-tests)
(require racket/local)

; ex 1.15
; duple: number X -> (listof X)
; Purpose: returns a list containing n copies of x
(define (duple n x)
  (if (equal? n 1) (list x) (cons x (duple (- n 1) x))))

(check-expect (duple 2 3) '(3 3))
(check-expect (duple 3 "s") '("s" "s" "s"))

; ex 1.16
; invert: (listof (listof symbol)) -> (listof (listof symbol))
; Purpose: to invert the list of 2-lists and return a list with each 2-list reversed
(define (invert los)
  (if (null? los) '()
      (cons (list (cadr (car los)) (car (car los))) (invert (cdr los)))))

(check-expect (invert '((a 1) (a 2) (1 b) (2 b))) '((1 a) (2 a) (b 1) (b 2)))


; ex 1.17
; down: (listof X) -> (listof X)
; Purpose: wraps parentheses around each top-level element of lst 
(define (downn lst)
  (if (null? lst) '() (cons (list (car lst)) (downn (cdr lst)))))

(check-expect (downn '(1 2 3)) '((1) (2) (3)))
(check-expect (downn '((a) (fine) (idea))) '(((a)) ((fine)) ((idea))))
(check-expect (downn '(a (more (complicated)) object)) '((a) ((more (complicated))) (object)))

; ex 1.18
; swapper: symbol symbol slist -> slist
; Purpose: to replace all occurences of s1 with s2 and s2 with s1 in the given list
(define (swapper s1 s2 slist)
  
  (define (subst-new-exp s1 s2 sexp)
    (if (symbol? sexp)
        (if (eqv? s1 sexp)
            s2
            (if (eqv? s2 sexp)
                s1
                sexp))
        (swapper s1 s2 sexp)))
  
  (if (null? slist)
      '()
      (cons (subst-new-exp s1 s2 (car slist))
            (swapper s1 s2 (cdr slist)))))
            

(check-expect (swapper 'a 'b '(a b c d)) '(b a c d))
(check-expect (swapper '(a b) 'b '(c (a b) d c d)) '(c (a (a b)) d c d))
(check-expect (swapper 'a '(b c) '(a a a a)) '((b c) (b c) (b c) (b c)))
(check-expect (swapper 'x 'y '((x) y (z (x)))) '((y) x (z (y))))


; ex 1.19
; list-set: (listof X) number X -> (listof X)
; Purpose: to replace the nth element of the list with x
(define (list-set lst n x)
  (map (lambda (i)
         (if (equal? (list-ref lst n) i) x i)) lst))

(check-expect (list-set '(a b c d) 2 '(1 2)) '(a b (1 2) d))
(check-expect (list-set '(1 2 3 4 5) 1 '(a b c)) '(1 (a b c) 3 4 5))

;ex 1.20
(define (foldl proc init lst)
  (cond [(null? lst) init]
        [else (proc (car lst) (foldl proc init (cdr lst)))]))

; count-occuurences: symbol slist -> number
; Purpose:to count the number of occurences of s in a list of symbols
(define (count-occurences s slist)
  (local [; helper: s-exp -> number 
          ; Purpose: to output 1 if the s-exp is equal to or contains s, and 0 otherwise 
          (define (helper sexp)
            (cond [(and (symbol? sexp)
                        (equal? s sexp)) 1]
                  [(list? sexp) (foldl + 0 (map (lambda (x) (if (equal? x s) 1 0)) sexp))]
                  [else 0]))]
    
    (cond [(null? slist) 0]
          [else (+ (helper (car slist)) (count-occurences s (cdr slist)))])))

(check-expect (count-occurences 'b '((i s) a (b e l) (l a))) 1)
(check-expect (count-occurences 'a '(a b c d)) 1)
(check-expect (count-occurences 'l '((k i l l) (m e))) 2)
(check-expect (count-occurences 'x '()) 0)

; count-occurences2: symbol slist -> number
; Purpose: To count the occurences of s in an slist (using accumulator)
(define (count-occurences2 s slist)
  (local [; helper: symbol slist number -> number
          ; Purpose: To count the occurences of s in an slist
          ; ACCUM-INV: The accumulated occurences of s
          (define (helper s slist accum)
            (cond [(null? slist) accum]
                  [(equal? s (car slist)) (helper s (cdr slist) (+ 1 accum))]
                  [(list? (car slist)) (helper s (cdr slist) (+ (foldl + 0 (map (lambda (x) (if (equal? x s) 1 0)) (car slist))) accum))] 
                  [else (helper s (cdr slist) accum)]))]
    (helper s slist 0)))

(check-expect (count-occurences2 'b '((i s) a (b e l) (l a))) 1)
(check-expect (count-occurences2 'a '(a b c d)) 1)
(check-expect (count-occurences2 'l '((k i l l) (m e))) 2)
(check-expect (count-occurences2 'x '()) 0)

; ex 1.21
; product: (listof symbols) (listof symbols) -> (listof (listof symbols)) 
; Purpose: To find the cartesian product of two sets 
(define (product sos1 sos2)
  (cond [(or (null? sos1)
             (null? sos2)) '()]
        [else (append (map (lambda (i) (list (car sos1) i)) sos2)
                      (product (cdr sos1) sos2))]))

(check-expect (product '(a b c) '(x y)) '((a x) (a y) (b x) (b y) (c x) (c y)))
(check-expect (product '() '(x y)) '())
(check-expect (product '(h h h) '(a a)) '((h a) (h a) (h a) (h a) (h a) (h a)))

; ex 1.22
; filter-in: predicate list -> list
; Purpose: to output the elements of a list that satisfy the predicate
(define (filter-in pred lst)
  (cond [(null? lst) '()]
        [(pred (car lst)) (cons (car lst) (filter-in pred (cdr lst)))]
        [else (filter-in pred (cdr lst))]))

(check-expect (filter-in number? '(a b (1 2))) '())
(check-expect (filter-in number? '(a 2 (1 3) b 7)) '(2 7))
(check-expect (filter-in symbol? '(a 2 (1 3) b 7)) '(a b))

; ex 1.23
; list-index: predicate list -> number or boolean 
; Purpose: To count the number of elements in the list that satisfy the predicate, if any
(define (list-index pred lst)
  (local [;helper: pred lst number -> number or boolean
          ;Purpose: To count the number of elements in the list that satisfy the predicate, if any 
          ;ACCUM-INV: accum = accumulated number of elements that satisfy the predicate
          (define (helper pred lst accum)
            (cond [(null? lst) #f]
                  [(pred (car lst)) accum]
                  [else (helper pred (cdr lst)(+ 1 accum))]))]
    (helper pred lst 0)))

(check-expect (list-index symbol? '(1 2 (a))) #f)
(check-expect (list-index symbol? '(b 1 2 (a))) 0)
(check-expect (list-index number? '(a b 3 e)) 2)

; ex 1.24
; every?: predicate list -> boolean
; Purpose: to output true if all elements of the list satisfy the predicate
(define (every? pred lst)
  (cond [(null? lst) #t]
        [(not (pred (car lst))) #f]
        [else (every? pred (cdr lst))]))

(check-expect (every? number? '(a b c 3 e)) #f)
(check-expect (every? number? '(1 2 3 4)) #t)
(check-expect (every? symbol? '(a b c d)) #t)

; ex 1.25
; exists?: prediciate list -> boolean
; Purpose: To output true if any element of the list satisfies the predicate 
(define (exists? pred lst)
  (cond [(null? lst) #f]
        [(pred (car lst)) #t]
        [else (exists? pred (cdr lst))]))

(check-expect (exists? number? '(a b c 3 e)) #t)
(check-expect (exists? symbol? '(a b c d e)) #t)


; ex 1.27 
; flatten: slist -> (listof symbols)
; Purpose: To remove all inner parentheses from an slist
(define (flatten slist)
  (local [;helper: sexp -> sexp
          ;Purpose: To remove blah blah blah
          (define (helper sexp)
            (cond [(symbol? sexp) (list sexp)]
                  [else sexp]))]
    (cond [(null? slist) '()]
          [else (append (helper (car slist)) (flatten (cdr slist)))])))
             
(check-expect (flatten '(a (b c) d)) '(a b c d))
(check-expect (flatten '((a b) (c d) e (f g))) '(a b c d e f g))

; ex 1.28
; merge: (listof integers) (listof integers) -> (listof integers)
; Purpose: To output the sorted appended list of two lists 
(define (merge loi1 loi2)
  (sort (append loi1 loi2)))

(check-expect (merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
(check-expect (merge '(35 62 81 90 91) '(3 83 85 90)) '(3 35 62 81 83 85 90 90 91))

; ex 1.29
; sort: (listof integer) -> (listof integer)
; Purpose: sort the given list in non-decreasing order 
(define (sort aloi)
  (local [ ; (listof integer) integer --> (listof integer)
          ; Purpose: To extract the integers <= to the given integer from the given loi
          (define (smaller= L pivot)
            (filter-in (lambda (i) (<= i pivot)) L))
        
          ; (listof integer) integer --> (listof integer)
          ; Purpose: To extract the integers > to the given integer from the given loi
          (define (larger L pivot)
            (filter-in (lambda (i) (> i pivot)) L))]
  
    (cond[(null? aloi) aloi]
         [else (append (sort (smaller= (cdr aloi) (car aloi)))
                       (cons (car aloi)
                             (sort (larger (cdr aloi) (car aloi)))))])))

(check-expect (sort '(8 2 5 2 3)) '(2 2 3 5 8))

; ex 1.31
(define BT1 1)
(define BT2 '(foo 1 2))
(define BT3 '(bar 1 (foo 1 2)))
(define BT4 '(baz
              (bar 1 (foo 1 2))
              (biz 4 5)))

; leaf
(define (leaf i) i)

; interior-node
(define (interior-node s lson rson)
  (cons s (cons lson rson)))

; leaf? 
(define leaf? integer?)

; lson, rson
(define lson cadr)
(define rson caddr)

; contents-of
(define (contents-of bt)
  (if (leaf? bt) bt (car bt)))

; ex. 1.32
; double-tree: bt -> bt
(define (double-tree bt)
  (local [(define (helper e)
            (if (leaf? e) (* 2 e) (map (lambda (x) (if (leaf? x) (* 2 x) x)) e)
                ))]
    (cond [(leaf? bt) (* 2 bt)]
          [else (map helper bt)]))) 

(define (double-tree2 bt) 
  (if (leaf? bt) (* (contents-of bt) 2)
      (interior-node (contents-of bt)
                     (double-tree (lson bt))
                     (double-tree (rson bt)))))

; ex. 1.33
; mark-leaves-with-red-depth: bt -> bt
; Purpose: 
(define (mark-leaves-with-red-depth bt)
  (local [; helper: bt -> bt 
          ; Purpose: 
          ; ACCUM-INV:
          (define (helper bt accum)
            (cond [(leaf? bt) accum]
                  [(equal? 'red (contents-of bt)) (interior-node (contents-of bt)
                                                                 (helper (lson bt) (+ 1 accum))
                                                                 (helper (rson bt) (+ 1 accum)))]
                  [else (interior-node (contents-of bt)
                                       (helper (lson bt) accum)
                                       (helper (rson bt) accum))]))]
    (helper bt 0)))

(define BT-TEST (interior-node 'red (interior-node 'bar
                                                   (leaf 26)
                                                   (leaf 12))
                               (interior-node 'red
                                              (leaf 11)
                                              (interior-node 'quux
                                                             (leaf 117)
                                                             (leaf 14)))))

(check-expect (mark-leaves-with-red-depth BT-TEST) (interior-node 'red (interior-node 'bar
                                                                                      (leaf 1)
                                                                                      (leaf 1)) 
                                                                  (interior-node 'red
                                                                                 (leaf 2)
                                                                                 (interior-node 'quux
                                                                                                (leaf 2)
                                                                                                (leaf 2)))))

(define BST1 '(14 (7 () (12 () ()))
                  (26 (20 (17 () ()) ())
                      (31 () ()))))

; ex. 1.34
; path: integer bst -> (listof symbol)
; Purpose: Find the path to n in the given bst
(define (path n bst)
  ; helper: n bst (listof symbol) -> (listof symbol)
  ; Purpose:
  ; ACCUM-INV: accumulated "turns" so far in path
  (define (helper n bst accum)
    (cond [(= n (car bst)) (reverse accum)] ; if the root of the BST is n, return path
          [(< n (car bst)) ; if n is less than the root, 
           (helper n (cadr bst) (cons 'left accum))] ; traverse the left branch and add a left turn to accum
          [else (helper n (caddr bst) (cons 'right accum))])) ; otherwise, traverse right branch and add a right turn
          
  (helper n bst '()))

(check-expect (path 17 BST1) '(right left left)) 

; ex. 1.35
; number-leaves: bt -> bt
; Purpose: count the leaves 
#;(define (number-leaves bt)
  ; helper: bt bt -> bt
  ; Purpose:
  ; ACCUM-INV: new bt with leaves marked
  (define (helper bt accum)
    (cond [(leaf? bt) ; if bt is just a leaf
           (cons (leaf accum) (+ 1 accum))] 
          [else (cons (interior-node (contents-of bt) ; original root
                                     (car (helper (lson bt) accum))
                                     (list (car (helper (rson bt) (cdr (helper (lson bt) accum))))))
                      (cdr (helper (rson bt) (cdr (helper (lson bt) accum)))))]))
  (car (helper bt 0)))

(define BT-TEST2 (interior-node 'foo
                                (interior-node 'bar
                                               (leaf 26)
                                               (leaf 12))
                                (interior-node 'baz
                                               (leaf 11)
                                               (interior-node 'quux
                                                              (leaf 117)
                                                              (leaf 14))))) 

#;(check-expect (number-leaves BT-TEST2) '(foo
                                         (bar 0 1)
                                         (baz
                                          2
                                          (quux 3 4))))

(test)
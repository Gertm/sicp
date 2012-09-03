#lang racket

(provide (all-defined-out))
(require racket/trace)

(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Exercise 2.24.  Suppose we evaluate the expression (list 1 (list 2 (list 3 4))).
;; Give the result printed by the interpreter, the corresponding box-and-pointer structure,
;; and the interpretation of this as a tree (as in figure 2.6).

;; (1 (2 (3 4)))
;; see piece of paper on desk


;; Exercise 2.25.  Give combinations of cars and cdrs that will pick 7 from each of the following lists:

;; (1 3 (5 7) 9)
;; ((7))
;;(1 (2 (3 (4 (5 (6 7))))))

(define l225_1 '(1 3 (5 7) 9))
(car (cdr (car (cdr (cdr l225_1)))))
(car (car '((7))))
(define l225_3 '(1 (2 (3 (4 (5 (6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l225_3))))))))))))

;; Exercise 2.26.  Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

;; What result is printed by the interpreter in response to evaluating each of the following expressions:

(append x y)
(cons x y)
(list x y)
;; ==>
'(1 2 3 4 5 6)
'((1 2 3) 4 5 6)
'((1 2 3) (4 5 6))

;; Exercise 2.27.  Modify your reverse procedure of exercise 2.18 to produce
;; a deep-reverse procedure that takes a list as argument and returns as its
;; value the list with its elements reversed and with all sublists deep-reversed
;; as well. For example,

;; (define x (list (list 1 2) (list 3 4)))

;; x
;; ((1 2) (3 4))

;; (reverse x)
;; ((3 4) (1 2))

;; (deep-reverse x)
;; ((4 3) (2 1))

 (define (deep-reverse items) 
   (cond ((null? items) '()) 
         ((not (pair? items)) items) 
         (else (append (deep-reverse (cdr items))  
                       (list (deep-reverse (car items))))))) 
;; this uses append.. might not be the most optimal way of doing this.
;; but it works. Not sure we already saw 'append' at this stage though.



;; Exercise 2.28.  Write a procedure fringe that takes as argument a tree
;; (represented as a list) and returns a list whose elements are all the
;; leaves of the tree arranged in left-to-right order. For example,

(define e228 (list (list 1 2) (list 3 4)))

(define (atom? a) (not (pair? a)))

(define (first-attempt-fringe x)
  (define (iter x result)
    (cond ((null? x) result)
          ((atom? x) (list x))
          ((null? (car x)) (iter (cdr x) result))
          ((atom? (car x)) (iter (cdr x) (cons (car x) result)))
          ((pair? (car x)) (iter (cons (fringe (car x)) (fringe (cdr x))) result))))
  (iter x '()))
;; ok, this is WAAAY too complicated.

;; again with append this time.
(define (fringe x) 
   (cond ((null? x) '())            ;; These are the only two base cases,
         ((not (pair? x)) (list x)) ;; the empty list and the atom.
         (else (append (fringe (car x)) (fringe (cdr x))))))  ;; drill down to the base cases.

;;(fringe e228)
;; (1 2 3 4)

;; (fringe (list e228 228))

;; (1 2 3 4 1 2 3 4)



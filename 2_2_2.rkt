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


;; Exercise 2.29.  A binary mobile consists of two branches, a left branch and a right branch.
;; Each branch is a rod of a certain length, from which hangs either a weight or another
;; binary mobile. We can represent a binary mobile using compound data by constructing
;; it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a length (which must be a number) together with a structure,
;; which may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))


(define mobile (make-mobile (make-branch 3 4) (make-branch 2 (make-mobile (make-branch 2 1) (make-branch 1 5)))))
(define balmobile (make-mobile (make-branch 2 (make-mobile (make-branch 2 1) (make-branch 2 1)))
                            (make-branch 2 (make-mobile (make-branch 2 1) (make-branch 2 1)))))
;; a.  Write the corresponding selectors left-branch and right-branch, which return
;; the branches of a mobile, and branch-length and branch-structure, which return the components of a branch.

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

;; b.  Using your selectors, define a procedure total-weight that returns the total weight of a mobile.

(define (branch-weight branch)
  (let ((bs (branch-structure branch)))
    (if (list? bs)
        (total-weight bs)
        bs)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

;; c.  A mobile is said to be balanced if the torque applied by its top-left
;; branch is equal to that applied by its top-right branch (that is, if the
;; length of the left rod multiplied by the weight hanging from that rod is
;; equal to the corresponding product for the right side) and if each of the
;; submobiles hanging off its branches is balanced. Design a predicate that
;; tests whether a binary mobile is balanced.

(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch) (branch-weight branch)))
  (define (same-torque? branch otherbranch)
    (= (torque branch) (torque otherbranch)))
  (define (has-sub-mobile? branch)
    (let ((bs (branch-structure branch)))
      (if (list? bs)
          #t
          #f)))
  (let ((lb (left-branch mobile))
        (rb (right-branch mobile)))
    (if (same-torque? lb rb)
        (and (if (has-sub-mobile? lb) (balanced? (branch-structure lb)) #t)
             (if (has-sub-mobile? rb) (balanced? (branch-structure rb)) #t))
        #f)))

  ;; d.  Suppose we change the representation of mobiles so that the constructors are

  ;; (define (make-mobile left right)
  ;;   (cons left right))
  ;; (define (make-branch length structure)
  ;;   (cons length structure))

  ;; How much do you need to change your programs to convert to the new representation?
;; cadr -> cdr
;; list? -> pair?

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

;; Exercise 2.30.  Define a procedure square-tree analogous to the
;; square-list procedure of exercise 2.21. That is, square-list should
;; behave as follows:

;; (square-tree
;;  (list 1
;;        (list 2 (list 3 4) 5)
;;        (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))

;; Define square-tree both directly (i.e., without using any higher-order
;; procedures) and also by using map and recursion.
(define (square x) (* x x))

(define (square-tree-direct tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-direct (car tree))
                    (square-tree-direct (cdr tree))))))

(square-tree-direct (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree-map tree)
  (map (lambda (sub-tree) (if (pair? sub-tree)
                              (square-tree-map sub-tree)
                              (square sub-tree)))
       tree))
            
(square-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))

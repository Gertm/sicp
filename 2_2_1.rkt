#lang racket

(provide (all-defined-out))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

;; Exercise 2.17.  Define a procedure last-pair that returns the list
;; that contains only the last element of a given (nonempty) list:
;; (last-pair (list 23 72 149 34))
;; (34)

;; (34) ~= (cons 32 '())
(define (last-pair lst)
  (if (null? (cddr lst))
      (cdr lst)
      (last-pair (cdr lst))))

(last-pair (list 23 72 149 34))

;; Exercise 2.18.  Define a procedure reverse that takes a list as
;; argument and returns a list of the same elements in reverse order:

;; (reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)

(define (reverse lst)
  (define (helper lst rst)
    (if (null? lst)
        rst
        (helper (cdr lst) (cons (car lst) rst))))
  (helper lst (list)))


;; Exercise 2.20.  The procedures +, *, and list take arbitrary numbers of
;; arguments. One way to define such procedures is to use define with
;; dotted-tail notation. In a procedure definition, a parameter list that
;; has a dot before the last parameter name indicates that, when the procedure
;; is called, the initial parameters (if any) will have as values the initial
;; arguments, as usual, but the final parameter's value will be a list of any
;; remaining arguments. For instance, given the definition

;; (define (f x y . z) <body>)

;; the procedure f can be called with two or more arguments. If we evaluate

;; (f 1 2 3 4 5 6)

;; then in the body of f, x will be 1, y will be 2, and z will be the list (3 4 5 6). Given the definition

;;(define (g . w) <body>)

;; the procedure g can be called with zero or more arguments. If we evaluate

;; (g 1 2 3 4 5 6)

;; then in the body of g, w will be the list (1 2 3 4 5 6).11

;; Use this notation to write a procedure same-parity that takes one or more
;; integers and returns a list of all the arguments that have the same even-odd
;; parity as the first argument. For example,

(define (same-parity . x)
  (define (get-parity x)
    (if (even? x) even? (lambda (x) (not (even? x)))))
  (define (get-same lst fn result)
    (if (null? lst)
        result
        (if (fn (car lst))
            (get-same (cdr lst) fn (cons (car lst) result))
            (get-same (cdr lst) fn result))))
  (reverse (get-same x (get-parity (car x)) '())))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)

;; 23:27:54 <+Quadrescence> Gertm, It would be more advantageous to write (same-parity x . xs)
;; 23:28:00 <+Quadrescence> Gertm, that way one argument is absolutely required

;; Exercise 2.21.  The procedure square-list takes a list of numbers as argument and returns a
;; list of the squares of those numbers.


;; (square-list (list 1 2 3 4))
;; (1 4 9 16)

;; Here are two different definitions of square-list.
;; Complete both of them by filling in the missing expressions:


;; (define (square-list items)
;;   (if (null? items)
;;       nil
;;       (cons <??> <??>)))
;; (define (square-list items)
;;   (map <??> <??>))

(define (square x)
  (* x x))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))
(define (square-list2 items)
  (map square items))

;; exercise 2.22
;; Louis Reasoner tries to rewrite the first square-list procedure
;; of exercise 2.21 so that it evolves an iterative process:

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things) 
;;               (cons (square (car things))
;;                     answer))))
;;   (iter items nil))

;; Unfortunately, defining square-list this way produces the answer
;; list in the reverse order of the one desired. Why?
;; -> He's adding elements to the front of the list as he goes.
;; Louis then tries to fix his bug by interchanging the arguments to cons:

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things)
;;               (cons answer
;;                     (square (car things))))))
;;   (iter items nil))

;; This doesn't work either. Explain.
;; -> He's constructing a list the wrong way, it will just result in a dotted pair at the end.

;; Exercise 2.23.  The procedure for-each is similar to map. It takes as arguments a procedure
;; and a list of elements. However, rather than forming a list of the results, for-each just
;; applies the procedure to each of the elements in turn, from left to right. The values
;; returned by applying the procedure to the elements are not used at all -- for-each is used
;; with procedures that perform an action, such as printing. For example,

;; (for-each (lambda (x) (newline) (display x))
;;           (list 57 321 88))
;; 57
;; 321
;; 88

;; The value returned by the call to for-each (not illustrated above) can be
;; something arbitrary, such as true. Give an implementation of for-each.

(define (for-each fn lst)
  (define (helper fn lst result)
    (if (null? lst)
        #t
        (helper fn (cdr lst) (fn (car lst)))))
  (helper fn lst '()))

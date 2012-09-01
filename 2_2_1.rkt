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



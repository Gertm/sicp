#lang typed/racket

(provide (all-defined-out))

(: square (Integer -> Integer))
(define (square x) (* x x))

(: sum-odd-squares ((Listof Integer) -> Integer))
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)  
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(: enumerate-interval (Integer Integer -> (Listof Integer)))
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;; Exercise 2.33.  Fill in the missing expressions to complete the following
;; definitions of some basic list-manipulation operations as accumulations:

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))


;; Exercise 2.34.  Evaluating a polynomial in x at a given value of x
;; can be formulated as an accumulation. We evaluate the polynomial

;; using a well-known algorithm called Horner's rule, which structures
;; the computation as

;; In other words, we start with an, multiply by x, add an-1, multiply
;; by x, and so on, until we reach a0 Fill in the following template
;; to produce a procedure that evaluates a polynomial using Horner's
;; rule.  Assume that the coefficients of the polynomial are arranged
;; in a sequence, from a0 through an.


;; (define (horner-eval x coefficient-sequence)
;;   (accumulate (lambda (this-coeff higher-terms) <??>)
;;               0
;;               coefficient-sequence))

;; For example, to compute 1 + 3x + 5x3 + x5 at x = 2 you would evaluate

;; (horner-eval 2 (list 1 3 0 5 0 1))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))


(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Exercise 2.35.  Redefine count-leaves from section 2.2.2 as an accumulation:

;; (define (count-leaves t)
;;   (accumulate <??> <??> (map <??> <??>)))
(define t '((3 4) (1 2)))

(define (count-leaves2 t)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x) (count-leaves2 x)
                             1)) t)))

;; Exercise 2.36.  The procedure accumulate-n is similar to accumulate
;; except that it takes as its third argument a sequence of sequences,
;; which are all assumed to have the same number of elements. It
;; applies the designated accumulation procedure to combine all the
;; first elements of the sequences, all the second elements of the
;; sequences, and so on, and returns a sequence of the results. For
;; instance, if s is a sequence containing four sequences, ((1 2 3) (4
;; 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s)
;; should be the sequence (22 26 30). Fill in the missing expressions
;; in the following definition of accumulate-n:

;; (define (accumulate-n op init seqs)
;;   (if (null? (car seqs))
;;       nil
;;       (cons (accumulate op init <??>)
;;             (accumulate-n op init <??>))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
;; Had to cheat and go look at the solution.
;; Let's try to figure out why this works.
;; AHA, at the last one, the lists are (() () ()), So (car seqs) results in '() => return '()
;; and this works nicely then.
;; Mapping over them lets you build the resulting list easily.


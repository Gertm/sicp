#lang racket

(provide (all-defined-out))

;; ex 2.4

(define (new-cons x y)
  (lambda (m) (m x y)))

(define (new-car z)
  (z (lambda (p q) p)))

;; (new-cons 'a 'b) -> (lambda (m) (m 'a 'b))
;; therefor:
;; (new-car z) -> (z (lambda (p q) p))
;;             -> ((lambda (m) (m 'a 'b)) (lambda p q) p)
;;             -> ((lambda (p q) p) 'a 'b)
;;             -> 'a

(define (new-cdr z)
  (z (lambda (p q) q)))


;; ex 2.5
;; Exercise 2.5.  Show that we can represent pairs of nonnegative integers
;; using only numbers and arithmetic operations if we represent the pair a
;; and b as the integer that is the product (2^a)*(3^b). Give the corresponding
;; definitions of the procedures cons, car, and cdr.

(define (25cons a b)
  (* (expt 2 a) (expt 3 b)))

;; now I need to extract a and b from that formula and we have the definitions of car and cdr


;; ex 2.6
;; Exercise 2.6.  In case representing pairs as procedures wasn't mind-boggling enough,
;; consider that, in a language that can manipulate procedures, we can get by without
;; numbers (at least insofar as nonnegative integers are concerned) by implementing 0
;; and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))))

;; This representation is known as Church numerals, after its inventor, Alonzo Church,
;; the logician who invented the calculus.

;; Define one and two directly (not in terms of zero and add-1).
;; (Hint: Use substitution to evaluate (add-1 zero)). Give a direct definition of the
;; addition procedure + (not in terms of repeated application of add-1).


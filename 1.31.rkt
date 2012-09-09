#lang racket

(provide (all-defined-out))
;; Exercise 1.31.  
;; a.  The sum procedure is only the simplest of a vast number of
;; similar abstractions that can be captured as higher-order
;; procedures. Write an analogous procedure called product that
;; returns the product of the values of a function at points over a
;; given range. Show how to define factorial in terms of product. Also
;; use product to compute approximations to using the formula
;;
;; pi   2·4·4·6·6·8·····
;; -- = ----------------
;; 4    3·3·5·5·7·7·····
;;
;; b.  If your product procedure generates a recursive process, write
;; one that generates an iterative process. If it generates an
;; iterative process, write one that generates a recursive process.

(define (cube a) (* a a a))

(define (inc a) (+ a 1))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (fact n)
  (define (id a) a)
  (product id 1 inc n))

(define (pi-over-4 i)
  (define (fraction n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (product fraction 1 inc i))

(define (prod-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (pi-over-4-iter i)
  (define (fraction n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (prod-iter fraction 1 inc i))

